#' Initialize stacker
#'
#' Creates a stacker governor containing the various information and data to run stacked generalization
#'
#' @param ... Initialized models. If blank, default versions of earth and gam are created.
#' @param data data table. Dataset to learned in stacking
#' @param indicator character vector. Name of the indicator (and by extension) the column name of the dependant variable
#' @param indicator_family character vector. Designates the statistical family that should be modeled. Usually 'binomial' or 'gaussian'
#' @param covariate_layers list of raster like objects. A named list of raster like objects of covariates
#' @param fe_equation character vector of an equation. The equation specifying the fixed effects portion of the model. It should match with the names
#' of covariate_layers.
#' @param centre_scale logical. Determines whether the covariate values are centered before being returned
#' @param time_var character vector. Name of the column denoting the time (e.g. period or year) of a given data point
#' @param time_scale numeric vector. List of years or times that the time var correlates to.
#' @param weight_col character vector. Denotes the column (if applicable) in the dataset that specifies the data weights
#' @param num_fold_cols numeric. Number of columns/interations for crossfold validation
#' @param num_folds numeric. The number of folds the data is split on.
#' @param cores numeric. The number of cores available for computation
#' @return Stacker governor object
#' @import data.table
#' @importFrom stats na.omit
#' @export
#'
init_stacker = function(..., data, indicator, indicator_family, covariate_layers, fe_equation, centre_scale = T, time_var = 'year',
                        time_scale = c(2000,2005,2010,2015), weight_col = NULL, num_fold_cols = 1, num_folds = 1, cores = 1){

  #if no child modules have been passed, make the default suite
  if(length(list(...)) == 0){
    m1 = init_earth()
    m2 = init_gam()
    models = list(m1, m2)
  } else {
    models = list(...)
  }

  #test to make sure all items in ... are stacker children
  #otherwise R with throw: Error in govner$models[[obj[["model_name"]]]] <- obj :
  #attempt to select less than one element in OneIndex

  #build the general settings
  general_settings = list(indicator = indicator, indicator_family = indicator_family,
    weight_col = weight_col, fe_equation = fe_equation, cores = cores,
    covs = format_covariates(fe_equation))

  #initialize the stacker object
  govner = structure(list(general_settings = general_settings), class = 'stacker_governor')

  #init dataset
  govner$data = data.table::copy(data)

  #make fold columns
  folds = make_stacking_folds(nrow(govner$data), numfolds = num_folds, numsets = num_fold_cols)
  govner$data = cbind(govner$data, folds)

  #extract covariates
  cov_vals = extract_covariates(xyt = govner$data[,c('longitude','latitude',time_var), with = F],
                                covariate_list = covariate_layers,
                                centre_scale = centre_scale,
                                time_var = time_var,
                                time_scale = time_scale)

  #add to dataset
  govner$data = cbind(govner$data, cov_vals[[1]])
  if(centre_scale) govner$cs_df = cov_vals[[2]]

  #omit missing rows and make row ids
  govner$data = na.omit(govner$data, cols = govner$general_settings$covs)
  govner$data[,('rid') := 1:nrow(govner$data)]

  #add to general settings
  govner$general_settings$fold_cols = names(folds)
  govner$general_settings$fold_ids = 1:num_folds

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


  return(govner)
}
#' Initialize an earth model
#'
#' Creates an object designed to be passed to init_stacker that describes an earth model be added to the stacking ensemble.
#'
#' @param model_name name of the earth model
#' @param arguments named list. Arguments to be passed to the earth function. See help earth::earth for more information
#' @return named list of lists with the parameters required to run an earth model
#' @export
#'
init_earth = function(model_name = 'earth', arguments = list(degree = NULL, nk = NULL)){
  model = list(model_name = model_name, model_type = 'earth', args = arguments)
  return(model)
}
#' Initialize a gam model
#'
#' Creates an object designed to be passed to init_stacker that describes an gam model be added to the stacking ensemble.
#'
#' @param model_name name of the gam model
#' @param arguments named list. Arguments to be passed to the gan function. See help mgcv::gam for more information
#' @param formula formula or formula-like. Formula to be passed to the gam call that overwrites the underlying formula builder.
#' This is useful if you want to have different covariates recieve different smoothing instructions
#' @return named list of lists with the parameters required to run an gam model
#' @export
#'
init_gam = function(model_name = 'gam',  arguments = list(spline_args = list(bs = 'ts', k = 3)), formula = NULL){
  model = list(model_name = model_name, model_type = 'gam', args = arguments, formula = formula)
  return(model)
}

#init brt
#' Initialize a brt model
#'
#' Creates an object designed to be passed to init_stacker that describes an brt model be added to the stacking ensemble.
#'
#' @param model_name name of the brt model
#' @param arguments named list. Arguments to be passed to the brt function.
#'                              Should only be used for options not captured by the params argument in the underlying xgboost function call.
#'                              See help xgboost::xgb.train for more information
#' @param params_arg named list. Arguments to be passed to the parameters argument of the xgboost::xgb.train function
#' @param nrounds numeric. Max number of iterations
#' @param emp_logit logical. Whether a binomial model should be be transformed using an emperical logit function instead of using the poisson approximation.
#'                           xgboost's binomial approach is exclusive to 1s and 0s
#' @return named list of lists with the parameters required to run an brt model
#' @export
#'
init_brt = function(model_name = 'brt',  arguments = list(), params_arg = list(nthread = 1), nrounds = 10, emp_logit = F){
  model = list(model_name = model_name, model_type = 'xgb.train', args = arguments, params_arg = params_arg, nrounds = nrounds, emp_logit = emp_logit)
  return(model)
}

#init rf

#' Initialize a penalized linear regression model
#'
#' Creates an object designed to be passed to init_stacker that describes an glm/penalized regression model be added to the stacking ensemble.
#'
#' @param model_name name of the penalized regression model model
#' @param arguments named list. Arguments to be passed to the glmnet function. See help glmnet::glmnet for more information.
#' The main arguement to be passed is alpha: 0 is ridge regression and 1 is lasso penalty. Between 0 and 1 refers to elastic net.
#' @param emp_logit logical. If family is binomial or poission, should the regression be run as gaussian (emp logit) or approximated as a poission with an offset.
#' This is required because the h2o package only accepts binomial in the 0/1 form.
#' @param standardize logical. Standardize numeric columns to have zero mean and unit variance. Defaults to False unlike the glmnet default settings.
#' This is useful if you want to have different covariates recieve different smoothing instructions
#' @return named list of lists with the parameters required to run an penalized regression model
#' @export
#'
init_penalized =function(model_name = 'pen',  arguments = list(alpha = 1), emp_logit = F, standardize = F){
  model = list(model_name = model_name, model_type = 'glmnet', args = arguments, emp_logit = emp_logit, standardize = standardize)
  return(model)
}
