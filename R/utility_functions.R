#Stacking Data Munging

#convert an equation into its consituent parts
format_covariates = function(covariates){
  if(length(covariates)==1){
    #split back into parts
    covariates = unlist(tstrsplit(covariates,"\\+"))
  }
  covariates = trimws(covariates)
  return(covariates)
}

#add additional terms to the covariate list
add_additional_terms= function(fes, add_terms = NULL){
  new_list = paste(unlist(sapply(c(fes, add_terms), function(x) format_covariates(x))), collapse = ' + ')
  return(new_list)
}

format_model_covs = function(covariates, additional_terms){
  return(format_covariates(add_additional_terms(covariates, additional_terms)))
}

get_model_type = function(st, model_name){
  st[['models']][[model_name]][['model_type']]
}

sanitize_parameters =function(x){
  x = x[!sapply(x, is.null)]
  return(x)
}

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

#find binary variables
#takes a data frame and returns the the names of binary variables
find_binary = function(df){
  df = as.data.table(df)
  n_uniq_vals = setNames(lapply(names(df), function(x) uniqueN(df[!is.na(get(x)),x,with=F])),names(df))
  binary_vars = names(n_uniq_vals[n_uniq_vals<=2])

  return(binary_vars)
}

#make folds for stacking
#numrows: number of data points/rows in the analytical dataset
#numfolds: number of folds within a set
#numsets: number of sets of folds
make_stacking_folds = function(numrows, numfolds = 8, numsets = 1, all_fold = T){
  fold_id = lapply(1:numsets, function(x) sample(cut(1:numrows,breaks=as.numeric(numfolds),labels=FALSE)))
  names(fold_id) = paste0('sfold_', 1:numsets)

  #generate an all fold
  if(all_fold){
    fold_id = cbind(data.frame(sfold_0 = rep(1, numrows)), fold_id)
  }

  return(data.frame(fold_id))
}

# A function to ensure an object has certain names
check_names = function(obj, required_names){
  stopifnot(sum(names(obj) %in% required_names)==length(required_names))
}

parseArgsS <- function(l) {
  # parse a list of additional arguments to smoothers in gamTrans
  stopifnot(is.list(l))
  l_string <- paste(names(l),
                    lapply(l, addQuotes),
                    sep = ' = ',
                    collapse = ', ')
  return (l_string)
}

build_parameter_grid = function(..., grid_type = 'ordered', model_type = NULL){

  model_params = list(...)

  #if they specify none, just return the first values
  if(is.null(grid_type) | grid_type == 'none') return(do.call(cbind,model_params)[1,])

  #if ordered is specified, simply cbind
  if(grid_type == 'ordered') return(do.call(cbind, model_params))

  #if all is specified, expand the grid
  if(grid_type == 'all') return(expand.grid(model_params))

}

#make test and train rows
make_test_train = function(data, fold_col, fold_id){
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