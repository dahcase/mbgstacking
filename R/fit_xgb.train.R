#' Fit boosted regression tree/gradient boosted machine
#'
#' Fit a boosted regression tree (or random forest given the proper set of options). See ??xgboost::xgb.train for more details
#'
#' @param st stacker governor. Stacking governer object with a penalized model initialized
#' @param model_name character vector. Name of the model to be run
#' @param fold_col character vector. Denotes the name of the column designating the fold for crossval
#' @param fold_id Numeric. Designates the value in fold col that should be held out
#' @param return_model_obj logical. Denotes whether the function should return the earth object or just predictions.
#' @param sub_cores numeric. Number of cores/processes to be used by xgboost.
#' @return List object with a data.table of predictions. If return_model_obj==T, the gam command and model object are returned as well
#' @import data.table
#' @importFrom stats predict
#' @export
#'
fit_xgb.train= function(st, model_name = 'brt',fold_col = NULL, fold_id = NULL, return_model_obj = F, sub_cores = 1){

  #fetch params, copy data and fetch indicator settings
  brt_params = st$models[[model_name]]
  indicator_family = st$general_settings$indicator_family
  indicator = st$general_settings$indicator

  #make set and train
  tetr = make_test_train(st$data, fold_col = fold_col, fold_id = fold_id)

  #make the train xgb.DMatrix
  if(indicator_family == 'binomial' | indicator_family == 'poisson'){
    if(brt_params$emp_logit){
      response_var = as.matrix(emplogit(st$data[tetr$train_rows,get(indicator)], st$data[tetr$train_rows,get('N')]))
      dm = xgboost::xgb.DMatrix(data = as.matrix(st$data[tetr$train_rows, st$general_settings$covs, with = F]),
                       label = response_var,
                       weight = as.matrix(st$data[tetr$train_rows,get('data_weight')]))
      indicator_family = 'gaussian'
    } else{
      dm = xgboost::xgb.DMatrix(data = as.matrix(st$data[tetr$train_rows, st$general_settings$covs, with = F]),
                       label = as.matrix(st$data[tetr$train_rows, indicator, with = F]),
                       weight = as.matrix(st$data[tetr$train_rows,get('data_weight')]))
      #add the log offset
      xgboost::setinfo(dm, "base_margin", log(st$data[tetr$train_rows, get('N')]))
      indicator_family = 'poisson'
    }

  }else{
    dm = xgboost::xgb.DMatrix(data = as.matrix(st$data[tetr$train_rows, st$general_settings$covs, with = F]),
                     label = as.matrix(st$data[tetr$train_rows, indicator, with = F]),
                     weight = as.matrix(st$data[tetr$train_rows,get('data_weight')]))
  }

  #sort out the objective
  if(indicator_family == 'poisson') iobject = 'count:poisson'
  if(indicator_family == 'gaussian') iobject = 'reg:linear'

  #build the params argument-- add the objective and nthread where not otherwise specified
  additional_params = list(objective = iobject, nthread = sub_cores)
  new_params = names(additional_params)[!names(additional_params) %in% names(brt_params$param_args)]
  brt_params$param_args = append(brt_params$param_args, additional_params[new_params])

  #model call
  command = list(
              params = brt_params$param_args,
              data = dm,
              nrounds = brt_params$nrounds,
              verbose = 0)
  command = append(command, sanitize_parameters(brt_params$args))

  #dedupe
  command = command[!duplicated(command)]


  #fit main model
  mod = do.call(xgboost::xgb.train, args = command)

  #transform test data into an xgb matrix
  newdata = xgboost::xgb.DMatrix(data = as.matrix(st$data[tetr$test_rows,st$general_settings$covs, with = F]))

  #create predictions
  output = predict(mod, newdata = newdata)
  if(brt_params$emp_logit == T) output = invlogit(output)

  output = data.table(rid = tetr$test_rows, prediction = output)

  #fix names
  names(output) = c('rid', paste0(model_name,".",fold_col,".",fold_id))

  #convert mod to raw to avoid handle/pointer issues
  mod = xgboost::xgb.save.raw(mod)


  if(return_model_obj){
    return(list(output, mod, command))
  } else {
    return(list(output))
  }

}
