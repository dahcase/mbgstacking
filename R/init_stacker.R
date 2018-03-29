#' Initialize stacker
#'
#' Creates a stacker governor containing the various information and data to run stacked generalization in IHME's MBG framework
#'
#' @param ... Initialized models. If blank, default versions of earth and gam are created.
#' @param data data table. Dataset to be machine learned.
#' @param inlist logical. Are the models being passed through ... already in a list format?
#' @param indicator character vector. Name of the indicator (and by extension) the column name of the dependant variable
#' @param indicator_family character vector. Designates the statistical family that should be modeled. Usually 'binomial' or 'gaussian'
#' @param covariate_layers list of raster like objects. A named list of raster like objects of covariates
#' @param fe_equation character vector of an equation. The equation specifying the fixed effects portion of the model. It should match with the names
#' of covariate_layers.
#' @param centre_scale logical. Determines whether the covariate values are centered/normalized before being returned. Binary variables are ignored.
#' @param time_var character vector. Name of the column denoting the time (e.g. period or year) of a given data point
#' @param time_scale numeric vector. List of years or times that the time var correlates to.
#' @param weight_col character vector. Denotes the column (if applicable) in the dataset that specifies the data weights
#' @param num_fold_cols numeric or character. Number of columns/interations for crossfold validation. if a character string, assume it refers to columns already existing in data.
#'                      They will be renamed to sfold_#
#' @param num_folds numeric. The number of folds the data is split on.
#' @param cores numeric. The number of cores available for parallel computation
#' @param sge_parameters object returned from init_sge. Provides sge parameters to govern submodel computation. If NULL, mclapply is used to run submodels instead
#' @return Stacker governor object
#' @import data.table
#' @importFrom stats na.omit
#' @export
#'
init_stacker = function(..., inlist = T, data, indicator, indicator_family, covariate_layers, fe_equation, centre_scale = T, time_var = 'year',
                        time_scale = c(2000,2005,2010,2015), weight_col = NULL, num_fold_cols = 1, num_folds = 1, cores = 1, sge_parameters = NULL){

  #if no child modules have been passed, make the default suite
  if(length(list(...)) == 0){
    m1 = init_earth()
    m2 = init_gam()
    models = list(m1, m2)
  } else{
    models = list(...)
  }

  if(inlist){
    models = models[[1]]
  }

  #test to make sure all items in ... are stacker children
  #otherwise R with throw: Error in govner$models[[obj[["model_name"]]]] <- obj :
  #attempt to select less than one element  in OneIndex

  #test to make sure data has x y and t

  #build the general settings
  general_settings = list(indicator = indicator, indicator_family = indicator_family,
                          weight_col = weight_col, fe_equation = fe_equation, cores = cores,
                          covs = format_covariates(fe_equation), sge_parameters = sge_parameters)

  #initialize the stacker object
  govner = structure(list(general_settings = general_settings), class = 'stacker_governor')

  #init dataset
  govner$data = data.table::copy(data)

  #make fold columns
  if(is.character(num_fold_cols)){
    #rename the fold columns
    folds = num_fold_cols

    #check to make sure all fold columns have the same number/fold identifiers
    if(length(folds)>1){
      fold_val_pos = lapply(folds, function(x) sort(unique(govner$data[,get(x)])))
      fvp_check = unlist(lapply(fold_val_pos[2:length(fold_val_pos)], function(x) all.equal(fold_val_pos[[1]], x)))

      if(!all(fvp_check)){
        stop('Some of your fold columns do not have the same fold identifiers. Please fix this.')
      }
    }
    fold_vals = unique(govner$data[,get(folds[1])])
    fold_names = folds

  }else if(num_fold_cols>0){
    folds = make_stacking_folds(nrow(govner$data), numfolds = num_folds, numsets = num_fold_cols)
    govner$data = cbind(govner$data, folds)
    fold_names = names(folds)
    fold_vals = unique(folds[,1])
  }else{
    fold_names = NA
    fold_vals = NA
  }


  #extract covariates
  cov_vals = extract_covariates(xyt = govner$data[,c('longitude','latitude',time_var), with = F],
                                covariate_list = covariate_layers,
                                centre_scale = centre_scale,
                                time_var = time_var,
                                time_scale = time_scale)

  #add to dataset
  if(nrow(govner$data) != nrow(cov_vals[[1]])){
    stop('Dataset does not have a companion row in the covariate extraction. Check time scale is specified properly')
  }
  govner$data = cbind(govner$data, cov_vals[[1]])
  if(centre_scale) govner$cs_df = cov_vals[[2]]

  #omit missing rows and make row ids
  start = nrow(govner$data)
  govner$data = na.omit(govner$data, cols = govner$general_settings$covs)
  end = nrow(govner$data)

  if(start != end){
    warnings(paste('Dropped', round(start/end,2)*100, 'percent of the dataset becausing of missing covariate values.'))
  }

  govner$data[,('rid') := 1:nrow(govner$data)]

  #add to general settings
  govner$general_settings$fold_cols = fold_names
  govner$general_settings$fold_ids = fold_vals

  #add time settings
  govner$general_settings$time_var = time_var
  govner$general_settings$time_scale = time_scale

  #set up the weight column
  if(!is.null(weight_col)){
    govner$data[,('data_weight') := govner$data[,get(weight_col)]]
  } else {
    govner$data[,('data_weight') := 1]
  }

  ##add the objects
  govner$models = list()
  for(obj in models){
    govner$models[[obj[['model_name']]]] <- obj
  }

  ##add covariate layer pointers
  govner$covariate_layers = covariate_layers

  #model grid
  govner$model_grid = make_model_grid(govner, add_parents = T)

  #add the model types
  govner$model_grid$model_type = apply(govner$model_grid[,'model_name'], MARGIN = 1, function(x){(
    paste0('fit_',get_model_type(govner, x)))
  })




  return(govner)
}
