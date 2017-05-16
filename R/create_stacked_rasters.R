#' Make rasters from the child model objects
#'
#' Using model fits from run_stacking_child_models, creates rasters
#' @param st stacker governer.
#' @param model_objects a named list of model objects
#' @param time_points numeric vector. the name/identifier/subset to be predicted-- on the scale of time_var
#' @import data.table
#' @export
#'
make_all_children_rasters = function(st, model_objects, time_points = NULL){

  #build a grid listing the years of analysis and submodels
  child_ras_grid = data.table::data.table(expand.grid(models = names(model_objects), time_scale = st$general_settings$time_scale, stringsAsFactors = F))

  #sort by model and then year
  data.table::setorderv(child_ras_grid, c('models', 'time_scale'))

  #make corrosponding time position variable (e.g. which raster it is likely to be)
  child_ras_grid = child_ras_grid[, ('time_position') := 1:length(st$general_settings$time_scale)]

  #keep only the time points we care about
  if(!is.null(time_points)) child_ras_grid = child_ras_grid[get('time_scale') %in% time_points,]

  #check to make sure ras_grid is greater than 0
  stopifnot(nrow(child_ras_grid)>0)

  #make rasters from the selected child models
  raster_objects = parallel::mclapply(1:nrow(child_ras_grid),
                            function(x) make_child_raster(
                              model_obj = model_objects[[child_ras_grid[x,get('models')]]],
                              model_settings = st$models[[child_ras_grid[x,get('models')]]],
                              covs = lapply(st$covariate_layers, function(cl) fetch_covariate_layer(cl, child_ras_grid[x, get('time_position')])),
                              cs_df = st$cs_df,
                              indicator_family = st$general_settings$indicator_family,
                              cores = st$general_settings$cores
                            ))

  #create raster bricks
  #make row ids on the child ras grid
  child_ras_grid = child_ras_grid[,('rid') := 1:.N]
  raster_objects = setNames(lapply(unique(child_ras_grid[,get('models')]), #for each model
                                          function(x) raster::brick(raster_objects[child_ras_grid[get('models') == x, get('rid')]])),
                            unique(child_ras_grid[,get('models')]))
  #raster_objects = mclapply(1:unique)
  return(raster_objects)
}


#' Make rasters from the child model objects
#'
#' Using model fits from run_stacking_child_models, creates rasters
#' @param model_obj a model object from stacking
#' @param model_settings list. result of a model init giving the instructions on how to fit the model
#' @param covs a named list of rasters,
#' @param cs_df center scaling df
#' @param indicator_family character. Model family
#' @param cores numeric. Cores available for use
#' @import data.table
#' @importFrom stats na.omit predict setNames
#'
make_child_raster = function(model_obj, model_settings = NULL,  covs, cs_df = NULL, indicator_family = 'binomial',  cores = 1){

  #convert rasters into a data table
  dm = data.table::data.table(raster::as.data.frame(raster::stack(covs), xy = T))

  #make a row id
  dm = dm[, ('row_id') := 1:.N]

  #create a template
  template = dm[, c('x','y','row_id'), with = F]

  #centre scale
  if(!is.null(cs_df)){
    cs_dm = centreScale(dm[,names(covs), with = F], df = cs_df)
    dm = cbind(dm[,c('x','y', 'row_id')], cs_dm)
  }

  #drop rows with NAs
  dm = na.omit(dm)
  good_rows = dm[,'row_id', with = F]

  #begin predicting rasters
  #if a gam
  if(inherits(model_obj, 'gam')){

    ret_obj = predict(model_obj, newdata =dm, type = 'response')
    ret_obj = data.table::data.table(ret_obj)
    setnames(ret_obj, 'ret_obj')

  } else if(inherits(model_obj, 'glmnet')){

    #glmnet requires a matrix
    dm = as.matrix(dm)
    ret_obj = predict(model_obj, newx = dm[,rownames(model_obj$beta)], s=model_obj$cv_1se_lambda, type = 'link')

    #if the family is binomial, back transform-- either from native logit or emperical logit
    if(indicator_family == 'binomial'){
      ret_obj = invlogit(ret_obj)
    }

    #convert to a data table
    ret_obj = data.table::data.table(ret_obj)
    setnames(ret_obj, 'ret_obj')

  } else if(inherits(model_obj, 'earth')){

    ret_obj = data.table(predict(model_obj, newdata=dm, type = 'response'))
    setnames(ret_obj, 'ret_obj')

  } else if (inherits(model_obj, 'xgb.Booster')){

    #create new data object for xgboost
    dm = xgboost::xgb.DMatrix(data = as.matrix(dm))

    ret_obj = data.table::data.table(ret_obj = predict(model_obj, newdata = dm))

    if(!is.null(model_settings)){
      if(model_settings$emp_logit == T) ret_obj = invlogit(ret_obj)
    }
  }

  #remove dm from meory
  rm(dm)

  #convert back into a raster
  ret_obj = cbind(good_rows, ret_obj)

  #restore to former glory
  ret_obj= merge(template, ret_obj, by = 'row_id', all.x =T)
  setorderv(ret_obj, 'row_id')
  ret_obj = raster::rasterFromXYZ(ret_obj[,c('x','y','ret_obj')], res = raster::res(covs[[1]]), crs=raster::crs(covs[[1]]))

  #return the object
  return(setNames(ret_obj,model_settings$model_name))

}

#' Make rasters from the child model objects
#'
#' Using model fits from run_stacking_child_models, creates rasters
#' @param ras raster-like. Raster like object
#' @param time_position numeric. Location on the time scale but reduced to units of 1:max(time_scale)
#'
#'
fetch_covariate_layer = function(ras, time_position = 1){
  if(class(ras) == 'RasterBrick' | class(ras) == "RasterStack"){
    return(ras[[time_position]])
  } else{
    return(ras)
  }
}
