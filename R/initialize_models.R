#' Initialize stacker
#'
#' Creates a stacker governor containing the various information and data to run stacked generalizetion
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
#' @param return_model_obj logical. Denotes whether the function should return the earth object or just predictions.
#' @param weight_col character vector. Denotes the column (if applicable) in the dataset that specifies the data weights
#' @param num_fold_cols numeric. Number of columns/interations for crossfold validation
#' @param num_folds numeric. The number of folds the data is split on.
#' @param cores numeric. The number of cores available for computation
#' @return Stacker governor object
#' @import data.table
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

  #build the general settings
  general_settings = list(indicator = indicator, indicator_family = indicator_family,
    weight_col = weight_col, fe_equation = fe_equation, cores = cores,
    covs = format_covariates(fe_equation))

  #initialize the stacker object
  govner = structure(list(general_settings = general_settings), class = 'stacker_governor')

  #init dataset
  govner$data = copy(data)

  #make fold columns
  folds = make_stacking_folds(nrow(govner$data), numfolds = num_folds, numsets = num_fold_cols, all_fold = F)
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
  govner$data[,rid := 1:nrow(govner$data)]

  #add to general settings
  govner$general_settings$fold_cols = names(folds)
  govner$general_settings$fold_ids = 1:num_folds

  #set up the weight column
  if(!is.null(weight_col)){
    govner$data[,data_weight := govner$data[,get(weight_col)]]
  } else {
    govner$data[,data_weight := 1]
  }

  ##add the objects
  govner$models = list()
  for(obj in models){
    govner$models[[obj[['model_name']]]] <- obj
  }

  return(govner)
}

#init earth model
init_earth = function(model_name = 'earth', arguments = list(degree = NULL, nk = NULL)){
  model = list(model_name = model_name, model_type = 'earth', args = arguments)
  return(model)
}

#init gam
init_gam = function(model_name = 'gam',  arguments = list(spline_args = list(bs = 'ts', k = 3)), formula = NULL){
  model = list(model_name = model_name, model_type = 'gam', args = arguments, formula = formula)
  return(model)
}

#init brt

#init rf

#init penalized
#alpha: 0 for ridge, 1 for lasso, in between for enet
init_penalized =function(model_name = 'pen',  arguments = list(alpha = 1), emp_logit = F, lambda_search = T){
  model = list(model_name = model_name, model_type = 'h2o.glm', args = arguments, emp_logit = emp_logit,lambda_search =lambda_search)
  return(model)
}
