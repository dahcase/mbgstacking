#' Initialize a penalized linear regression model
#'
#' Creates an object designed to be passed to init_stacker that describes an glm/penalized regression model be added to the stacking ensemble.
#'
#' @param model_name name of the penalized regression model model
#' @param arguments named list. Arguments to be passed to the glmnet function. See help glmnet::glmnet for more information.
#' The main arguement to be passed is alpha: 0 is ridge regression and 1 is lasso penalty. Between 0 and 1 refers to elastic net.
#' @param emp_logit logical. If family is binomial, should the regression be run as gaussian (empirical logit of cases/N)
#' @param standardize logical. Standardize numeric columns to have zero mean and unit variance. Defaults to False unlike the glmnet default settings.
#'                             This is set to F, because centre scaling/normalizing is a default preprocessing step
#' @return named list of lists with the parameters required to run an penalized regression model
#' @export
#'
init_penalized =function(model_name = 'pen',  arguments = list(alpha = 1), emp_logit = F, standardize = F){
  model = list(model_name = model_name, model_type = 'glmnet', args = arguments, emp_logit = emp_logit, standardize = standardize)
  return(model)
}
