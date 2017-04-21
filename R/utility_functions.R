#Stacking Data Munging
#' Format covariate equation
#'
#' Splits a formula like string into a vector where each item is a term
#'
#' @param covariates a formula-like string
#' @return A vector where each item is a term in the formula
#' @import data.table
#'
#convert an equation into its consituent parts
format_covariates = function(covariates){
  if(length(covariates)==1){
    #split back into parts
    covariates = unlist(tstrsplit(covariates,"\\+"))
  }
  covariates = trimws(covariates)
  return(covariates)
}

#' Add additional terms to the covariate equation
#'
#'
#' @param fes original equation
#' @param add_terms a vector of new terms to add to the covariate equation
#' @return the covariate/fixed effects equation with additional terms tacked on
#'
#convert an equation into its consituent parts
add_additional_terms= function(fes, add_terms = NULL){
  new_list = paste(unlist(sapply(c(fes, add_terms), function(x) format_covariates(x))), collapse = ' + ')
  return(new_list)
}
#' Format Model Covariates
#'
#' Returns the covariate equation as a vector with each item as a term of the equation with additional terms appended on
#'
#' @param covariates a formula-like string
#' @param additional_terms a string vector of new terms
#' @return A vector where each item is a term in the formula
#'
#convert an equation into its consituent parts
format_model_covs = function(covariates, additional_terms){
  return(format_covariates(add_additional_terms(covariates, additional_terms)))
}

#' Get Model Type
#'
#' Returns the model type/class from the stacker object given a model name
#'
#' @param st stacker object
#' @param model_name character string of the model name
#' @return A string with the model type
#' @import data.table
#'
get_model_type = function(st, model_name){
  st[['models']][[model_name]][['model_type']]
}

#' Sanitize parameters
#'
#' Given a list of lists (e.g. parameters guiding a stacker child model), remove all the NULL entries
#'
#' @param x a subset of the stacker object
#' @return subset of the stacker object (e.g. list of lists) with no null entries
#'
sanitize_parameters =function(x){
  x = x[!sapply(x, is.null)]
  return(x)
}

#' Emperical logit
#'
#' Given two vectors (success and N), calculate the emperical logistc. This is used to deal with cases when success/N
#' equals 0 or 1.
#'
#' This stack exchange explains the process: http://stats.stackexchange.com/questions/109702/empirical-logit-transformation-on-percentage-data
#'
#' @param success numeric vector. Binomial successes
#' @param N numeric vector. Same length as success. Represents the number of binomial trials
#' @param epsilon numeric. Offset value for instances where success/N are equal to 0 or 1. If left NULL, the function defaults to 1/2 the minimum observed value
#' @import data.table
#' @export
#'

emplogit = function(success, N, epsilon = NULL) {
  #http://stats.stackexchange.com/questions/109702/empirical-logit-transformation-on-percentage-data
  #squeeze in the edges
  tform = success/N

  #if epsilon is null, follow the instructions from the attached link
  if(is.null(epsilon)){
    epsilon = min(tform[tform >0 & tform <1])/2
  }

  tform[tform==0] = tform[tform == 0] + epsilon
  tform[tform==1] = tform[tform==1] - epsilon
  tform = log(tform/(1-tform))

  return(tform)
}

#' Find binary variables
#'
#' Given a data table, returns the names of the columns that have two or fewer responses.
#'
#' @param df A data table.
#' @import data.table
#'
find_binary = function(df){
  df = as.data.table(df)
  n_uniq_vals = setNames(lapply(names(df), function(x) uniqueN(df[!is.na(get(x)),x,with=F])),names(df))
  binary_vars = names(n_uniq_vals[n_uniq_vals<=2])

  return(binary_vars)
}

#' Make stacking folds
#'
#' Assigns rows to different folds to support the cross validation inherent in stacking.
#'
#' @param numrows numeric. The number of data points/rows in the dataset
#' @param numfolds numeric. The number of folds within a set
#' @param numsets numeric. The number of fold sets to return
#' @import data.table
#'

make_stacking_folds = function(numrows, numfolds = 8, numsets = 1){
  fold_id = lapply(1:numsets, function(x) sample(cut(1:numrows,breaks=as.numeric(numfolds),labels=FALSE)))
  names(fold_id) = paste0('sfold_', 1:numsets)

  #generate an all fold
  if(all_fold){
    fold_id = cbind(data.frame(sfold_0 = rep(1, numrows)), fold_id)
  }

  return(data.frame(fold_id))
}

#' Check names
#'
#' Ensure an object has certain names. Written as a test rather than something that returns T/F
#'
#' @param obj any object with a names attribute.
#' @param required_names character vector. The names an object must have to pass the test
#'

check_names = function(obj, required_names){
  stopifnot(sum(names(obj) %in% required_names)==length(required_names))
}

#' Parse spline arguments
#'
#' Assigns rows to different folds to support the cross validation inherent in stacking.
#'
#' @param l named list. A named list that when deconstructed can be used to tell mgcv:gam what to do
#'
parseArgsS <- function(l) {
  # parse a list of additional arguments to smoothers in gamTrans
  stopifnot(is.list(l))
  l_string <- paste(names(l),
                    lapply(l, addQuotes),
                    sep = ' = ',
                    collapse = ', ')
  return (l_string)
}

#' Build Parameter Grid
#'
#' Given a set of paramters (e.g. named list). Returns a data frame where each row is a set of parameters to inform
#' a given model
#'
#' @param ... ... Named list of parameters
#' @param grid_type character vector. One of ordered, none or all. None returns the first value for each set of parameters,
#' ordered returns a data frame where each row refers to the position as passed and all returns all combinations.
#'
build_parameter_grid = function(..., grid_type = 'ordered', model_type = NULL){

  model_params = list(...)

  #if they specify none, just return the first values
  if(is.null(grid_type) | grid_type == 'none') return(do.call(cbind,model_params)[1,])

  #if ordered is specified, simply cbind
  if(grid_type == 'ordered') return(do.call(cbind, model_params))

  #if all is specified, expand the grid
  if(grid_type == 'all') return(expand.grid(model_params))

}

#' Make Test and Train identifiers
#'
#' Assigns rows to test/train designations depending on fold_col/fold_id combination. If null, test and train are the entire dataset.
#'
#' @param data data.table. Dataset to be split into test and train. Requires a column called 'rid' which is the row id.
#' @param fold_col character vector. Name of a column in data from which the test/train split should be made
#' @param fold_id numeric vector. Value designating which rows (given fold_col) should be left out.
#' @import data.table
#'
make_test_train = function(data, fold_col =NULL, fold_id=NULL){
  #mark which rows need to be included and which don't (e.g. test and train)
  if(is.null(fold_col)) fold_col = NA
  if(is.null(fold_id)) fold_id = NA

  if(!is.na(fold_col)){
    if(!is.na(fold_id)){
      train_rows = data[get(fold_col) != fold_id,rid]
      test_rows = data[get(fold_col) == fold_id, rid]
    }
  } else{
    train_rows = data[,rid]
    test_rows = train_rows
  }

  return(list(train_rows = train_rows, test_rows = test_rows))
}
