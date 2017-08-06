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
    fold_vals = unique(govner$data[,get(folds[1])])
    fold_names = folds

  }else{
    folds = make_stacking_folds(nrow(govner$data), numfolds = num_folds, numsets = num_fold_cols)
    govner$data = cbind(govner$data, folds)
    fold_names = names(folds)
    fold_vals = unique(folds[,1])
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
  govner$data = na.omit(govner$data, cols = govner$general_settings$covs)
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


  return(govner)
}
#' Initialize an earth model
#'
#' Creates an object designed to be passed to init_stacker that describes an earth::earth model be added to the stacking ensemble.
#'
#' @param model_name name of the earth model
#' @param arguments named list. Arguments to be passed to the earth function. See ??earth::earth for more information
#' @return named list of lists with the parameters required to run an earth model
#' @export
#'
init_earth = function(model_name = 'earth', arguments = list(degree = NULL, nk = NULL)){
  model = list(model_name = model_name, model_type = 'earth', args = arguments)
  return(model)
}
#' Initialize a gam model
#'
#' Creates an object designed to be passed to init_stacker that describes an gam::gam model be added to the stacking ensemble.
#'
#' @param model_name name of the gam model
#' @param arguments named list. Arguments to be passed to the gan function. See ??mgcv::gam for more information
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
#' Creates an object designed to be passed to init_stacker that describes an xgboost::xgb.train model be added to the stacking ensemble.
#'
#' @param model_name name of the brt model
#' @param arguments named list. Arguments to be passed to the xgboost::xgb.train function.
#'                              Should only be used for options not captured by the params argument in the underlying xgboost function call.
#'                              See help xgboost::xgb.train for more information
#' @param params_arg named list. Arguments to be passed to the parameters argument of the xgboost::xgb.train function
#' @param nrounds numeric. Max number of iterations
#' @param binomial_evaluation one of 'prev', 'poisson', or 'emplogit'. Prev fits on indicator/N with reg:logistic evaluation. 
#' Poisson uses log(N) as an offset while modelling under a poisson. Emplogit transforms things and runs under gaussian family
#' @return named list of lists with the parameters required to run an brt model
#' @export
#'
init_brt = function(model_name = 'brt',  arguments = list(), params_arg = list(nthread = 1), nrounds = 10, binomial_evaluation = 'prev'){
  model = list(model_name = model_name, model_type = 'xgb.train', args = arguments, params_arg = params_arg, nrounds = nrounds, binomial_evaluation = binomial_evaluation)
  return(model)
}

#init rf
##to be implemented

#' Initialize a penalized linear regression model
#'
#' Creates an object designed to be passed to init_stacker that describes an glm/penalized regression model be added to the stacking ensemble.
#'
#' @param model_name name of the penalized regression model model
#' @param arguments named list. Arguments to be passed to the glmnet function. See help glmnet::glmnet for more information.
#' The main arguement to be passed is alpha: 0 is ridge regression and 1 is lasso penalty. Between 0 and 1 refers to elastic net.
#' @param emp_logit logical. If family is binomial, should the regression be run as gaussian (emp logit of cases/N)
#' @param standardize logical. Standardize numeric columns to have zero mean and unit variance. Defaults to False unlike the glmnet default settings.
#'                             This is set to F, because centre scaling/normalizing is a default preprocessing step
#' @return named list of lists with the parameters required to run an penalized regression model
#' @export
#'
init_penalized =function(model_name = 'pen',  arguments = list(alpha = 1), emp_logit = F, standardize = F){
  model = list(model_name = model_name, model_type = 'glmnet', args = arguments, emp_logit = emp_logit, standardize = standardize)
  return(model)
}

#' Initialize SGE parameters
#'
#' Creates an object designed to be passed to init_stacker that describes how the internal machinery should handle .
#'
#' @param working_folder file path. Location accessible to the computing cluster where scratch files can be saved
#' @param r_path file path. Full path to R
#' @param output_files character string. Location where the output (.o) files are saved
#' @param error_files character string. Location where the error (.e) files are save
#' @param project_name character string. Cluster project
#' @param other_options character string. Character string of additional options to pass to qsub
#' @param slots_per_job numeric. Denotes the number of slots to be requested by each submodel. If length>1, first item is passed to child models and second to raster creation
#' @param package_location character string. Denotes the location where mbgstacking is installed (to be passed to the qsubs)
#' @param repeat_iterations numeric: How many times should jobs be relaunched if they don't work the first time.
#' @param conda_activate file path. Denotes the location where a conda environment activate option is. Not implemented.
#' @param conda_env file path. File path to the name of the conda environment. Not implemented.
#' @param write_shell logical. Should the qsub be run via writing shell scripts. If F, qsubs are lanunched via the "-b y" flag
#' @return List of lists containing the input parameters to be passed to the stacker
#' @export
#'
init_sge = function(working_folder, r_path, output_files = NULL, error_files = NULL, project_name = NULL, other_options = NULL, slots_per_job = c(3,5), package_location = NULL, repeat_iterations = 0, conda_activate = NULL, conda_env = NULL, write_shell = F){

  output <- error <- project <- NULL

  #standardize file paths
  if(substring(working_folder, nchar(working_folder), nchar(working_folder))!='/') working_folder = paste0(working_folder, '/')

  #build the sge stuff
  if(!is.null(output_files)) output = paste('-o',output_files)
  if(!is.null(error_files)) error = paste('-e', error_files)
  if(!is.null(project_name)) project = paste('-P', project_name)

  #combine
  sge_command = c(output, error, project, other_options)
  sge_command = sge_command[!is.null(sge_command)]
  sge_command = paste(sge_command, collapse = " ")

  #slotting
  if(length(slots_per_job)>1){
    child_model_slots = slots_per_job[1]
    raster_slots = slots_per_job[2]
  } else{
    child_model_slots <- raster_slots <- slots_per_job
  }

  return(list(working_folder = working_folder, r_path = r_path, sge_command = sge_command, child_model_slots = child_model_slots, raster_slots = raster_slots,
              package_location = package_location, repeat_iterations = repeat_iterations, conda_activate = conda_activate, conda_env = conda_env, write_shell = write_shell))

}
