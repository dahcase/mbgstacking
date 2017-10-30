
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
