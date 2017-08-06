#' Make rasters from the child model objects
#'
#' Using model fits from run_stacking_child_models, creates rasters
#' @param model_obj a model object from stacking
#' @param model_settings list. result of a model init giving the instructions on how to fit the model
#' @param covs a named list of rasters,
#' @param cs_df center scaling df
#' @param indicator_family character. Model family
#' @import data.table
#' @importFrom stats na.omit predict setNames
#' @export
#'
make_child_raster = function(model_obj, model_settings = NULL,  covs, cs_df = NULL, indicator_family = 'binomial'){

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

  } else if (inherits(model_obj, 'xgb.Booster') | inherits(model_obj, 'raw')){

    if(class(model_obj)=='raw'){
      model_obj = xgboost::xgb.load(model_obj)
    }

    #create new data object for xgboost
    dm = xgboost::xgb.DMatrix(data = as.matrix(dm[,names(covs), with = F])) #not sure this subsetting is 100% strong

    ret_obj = data.table::data.table(ret_obj = predict(model_obj, newdata = dm))

    if(!is.null(model_settings)){
      if(model_settings$binomial_evaluation == 'emplogit') ret_obj = invlogit(ret_obj)
    }
  }

  #remove dm from memory
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
