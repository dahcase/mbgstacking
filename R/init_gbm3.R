
#init brt
#' Initialize a GBM3 brt model
#'
#' Creates an object designed to be passed to init_stacker that describes an gbm3::gbmt_fit model be added to the stacking ensemble.
#'
#' @param model_name name of the brt model
#' @param arguments named list. Arguments to be passed to the gbm3::gbmt_fit function.
#'                              Should only be used for options not captured by the params argument in the underlying xgboost function call.
#'                              See help xgboost::xgb.train for more information
#' @param training_params named list. Arguments to be passed to the gbm3::training_params function
#' @param nrounds numeric. Max number of iterations
#' @param binomial_evaluation one of 'prev', 'poisson', or 'emplogit'. Prev fits on indicator/N with reg:logistic evaluation.
#' Poisson uses log(N) as an offset while modelling under a poisson. Emplogit transforms things and runs under gaussian family
#' @return named list of lists with the parameters required to run an brt model
#' @export
#'
init_xgboost = function(model_name = 'xgboost',  arguments = list(), params_arg = list(nthread = 1), nrounds = 10, binomial_evaluation = 'prev'){
  model = list(model_name = model_name, model_type = 'xgb.train', args = arguments, params_arg = params_arg, nrounds = nrounds, binomial_evaluation = binomial_evaluation)
  return(model)
}
