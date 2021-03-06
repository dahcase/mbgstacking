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
#' @export
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
  n_uniq_vals = stats::setNames(lapply(names(df), function(x) uniqueN(df[!is.na(get(x)),x,with=F])),names(df))
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
build_parameter_grid = function(..., grid_type = 'ordered'){

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
#' @export
#'
make_test_train = function(data, fold_col =NULL, fold_id=NULL){
  #mark which rows need to be included and which don't (e.g. test and train)
  if(is.null(fold_col)|is.na(fold_col) | fold_col == "NA") fold_col = NA
  if(is.null(fold_id)|is.na(fold_id)) fold_id = NA

  if(!is.na(fold_col)){
    if(!is.na(fold_id)){
      train_rows = data[get(fold_col) != fold_id,get('rid')]
      test_rows = data[get(fold_col) == fold_id, get('rid')]
    }
  } else{
    train_rows = data[,get('rid')]
    test_rows = train_rows
  }

  return(list(train_rows = train_rows, test_rows = test_rows))
}

#' Logit transformation
#'
#' Given a numeric vector, the function applies the logit transformation: log(x/(1-x))
#'
#' @param x numeric vector.
#'
logit <- function(x) {
  log(x/(1-x))
}

#' Inverse logit transformation
#'
#' Given a numeric vector, the function applies the inverse logit transformation: exp(x)/(1+exp(x))
#'
#' @param x numeric vector.
#'
invlogit <- function(x) {
  exp(x)/(1+exp(x))
}

#' Center (e.g. normalize) data
#'
#' @param x data frame.
#' @param exclude character vector. List of columns in x to ignore (e.g. leave unstandardized)
#' @param na.rm logical. na.rm setting for colmeans and sd.
#'
getCentreScale <- function (x, exclude = NULL, na.rm = TRUE) {
  # get dataframe of centreing and scaling values to convert x
  # to the standard normal. exclude is an optional character vector
  # giving column names to exclude from scaling

  # get means and SDs for all columns
  df <- data.frame(name = colnames(x),
                   mean = colMeans(x, na.rm = na.rm),
                   sd = apply(x, 2, stats::sd, na.rm = na.rm))
  rownames(df) <- NULL

  # replace any zero standard deviations with 1
  # to avoid divide-by-zero errors
  df$sd[df$sd == 0] <- 1

  # if any named covariates are to be excluded, set mean to 0 and sd to 1
  if (!is.null(exclude)) {
    idx <- match(exclude, df$name)
    df$mean[idx] <- 0
    df$sd[idx] <- 1
  }

  return (df)
}

#' Center (e.g. normalize) data
#'
#' @param x data frame (or similiar). The data object
#' @param df data frame. data frame providing mean and standard deviations from getCentreScale
#' @param inverse logical. If T, x is unscaled rather than scaled/normalized
#'
centreScale <- function (x, df, inverse = FALSE) {
  # apply pre-calculated centreing/scaling to matrix x,
  # with fixed dataframe of means/sds df
  # or uncentre/unscale if inverse = TRUE

  # get the centreing/scaling dataframe if not available
  if (is.null(df))
    df <- getCentreScale(x)

  # get index to match up values with column names
  names <- colnames(x)
  idx <- match(names, df$name)

  if (any(is.na(idx))) {
    stop ('could not match up column names with the values in df')
  }

  df <- df[idx, ]

  # apply transformations
  if (!inverse) {
    # move to standard normal

    # centre
    x <- sweep(x, MARGIN = 2, STATS = df$mean, FUN = '-')
    # scale
    x <- sweep(x, MARGIN = 2, STATS = df$sd, FUN = '/')

  } else {
    # inverse case, move from standard normal to original

    # unscale
    x <- sweep(x, MARGIN = 2, STATS = df$sd, FUN = '*')
    # uncentre
    x <- sweep(x, MARGIN = 2, STATS = df$mean, FUN = '+')

  }

  return (x)

}

#' Add quotes
#'
#' @param x character string. Object to add quotes around
#' @export
#'
addQuotes = function (x)
{
  addQuotes_s(x)
}

#' Add quotes using "
#'
#' @param x character string. Object to add quotes around
#' @export
#'
addQuotes_d = function (x)
{
  if (is.character(x)) {
    x <- sprintf("\"%s\"", x)
  }
  return(x)
}

#' Add quotes using '
#'
#' @param x character string. Object to add quotes around
#' @export
#'
addQuotes_s = function (x)
{
  if (is.character(x)) {
    x <- sprintf("\'%s\'", x)
  }
  return(x)
}

#' Generate a model grid from a stacker object
#'
#' @param st stacker governor.
#' @param add_parents logical. Add the "parent" models (e.g. the full ones where we want the model object)
#' @export
#'
make_model_grid = function(st, add_parents = T){
  utils::globalVariables
  model_grid = data.table(expand.grid(
    model_name = names(st$models),
    fold_columns = st$general_settings$fold_cols,
    fold_ids = st$general_settings$fold_ids, stringsAsFactors = F),
    return_model_obj = F)

  #drop rows where fold is NA and return model object is false. This is to allow models without cross validation
  model_grid = model_grid[!is.na(fold_columns),]

  #add the main model runs
  if(add_parents){
    main_mods = data.table(model_name = names(st$models), return_model_obj = T)
    model_grid = rbind(model_grid,main_mods, fill = T)
  }

  return(unique(model_grid))

}

#' Create a blank job to pause R until the child jobs complete
#'
#' @param st stacker governor. Must have relevant sge settings
#' @param job_name character. Name of the holding job
#' @param hold_on_jobs character vector. The name of the jobs that this job should hold on.
#' @export
#'
sge_hold_via_sync = function(st, job_name = "holder", hold_on_jobs) {
  hold_jobs = paste('-hold_jid', paste(hold_on_jobs, collapse = ','))
  job_name = paste("-N", job_name)
  project_and_logs = st$general_settings$sge_parameters$sge_command
  slot_request = '-pe multi_slot 1'
  additional_flags = '-b y -sync y'
  command = shQuote('echo 1')

  qsub = paste('qsub', job_name, hold_jobs, project_and_logs, slot_request, additional_flags, command)
  system(qsub)
}

#' Make rasters from the child model objects
#'
#' Using model fits from run_stacking_child_models, creates rasters
#' @param ras raster-like. Raster like object
#' @param time_position numeric. Location on the time scale but reduced to units of 1:max(time_scale)
#' @export
#'
fetch_covariate_layer = function(ras, time_position = 1){
  if(class(ras) == 'RasterBrick' | class(ras) == "RasterStack"){
    return(ras[[time_position]])
  } else{
    return(ras)
  }
}


