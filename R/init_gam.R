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
