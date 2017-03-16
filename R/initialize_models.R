#init stacker
init_stacker = function(..., data, indicator, indicator_family, covariate_layers, fe_equation, centre_scale = T, time_var = 'year', time_scale = c(2000,2005,2010,2015), weight_col = NULL, num_fold_cols = 1, num_folds = 1, cores = 1, lib.loc = NULL){

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
    covs = format_covariates(fe_equation), lib.loc = lib.loc)

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
