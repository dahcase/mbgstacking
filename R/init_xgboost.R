
#init brt
#' Initialize a XGBOOST brt model
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
