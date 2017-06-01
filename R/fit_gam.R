#' Fit gam model
#'
#' Fit a generalized additive model using mgcv::gam. See ??gam for more details
#'
#' @param st stacker governor. Stacking governer object with an gam model initialized. See ??mbgstacking::init_gam for more info.
#' @param model_name character vector. Name of the model to be run
#' @param fold_col character vector. Denotes the name of the column designating the fold for crossval
#' @param fold_id Numeric. Designates the value in fold col that should be held out
#' @param return_model_obj logical. Denotes whether the function should return the earth object or just predictions.
#' @return List object with a data.table of predictions. If return_model_obj==T, the gam command and model object are returned as well
#' @import data.table
#' @importFrom stats predict
#' @export
#'
fit_gam = function(st, model_name = 'gam',fold_col = NULL, fold_id = NULL, return_model_obj = F){

  #subset the model parameters we need
  gam_params = st$model[[model_name]]
  indicator_family = st$general_settings$indicator_family
  indicator = st$general_settings$indicator

  #ge the spline args and then drop them from gam params so they don't get passed
  #to the function call
  spline_args = gam_params$args$spline_args
  gam_params$args$spline_args = NULL

  #get test and train
  tetr = make_test_train(st$data, fold_col = fold_col, fold_id = fold_id)

  #set the response variable
  if(st$general_settings$indicator_family=="binomial"){
    response <- cbind(success = st$data[tetr$train_rows, get(indicator)], failure = st$data[tetr$train_rows, get('N')] - st$data[tetr$train_rows, get(indicator)])
  } else{
    response = st$data[tetr$train_rows,get(st$general_settings$indicator)]
  }

  #build the gam call
  #if its prespecified, use it
  if(!is.null(gam_params$formula)){
    gam_form = gam_params$formula
  } else{
    #split out binary and non binary vars
    binary_vars = st$general_settings$covs[st$general_settings$covs %in% find_binary(st$data)]
    non_binary_vars = st$general_settings$covs[!st$general_settings$covs %in% binary_vars]
    gam_form = paste(paste0('s(',non_binary_vars,',',parseArgsS(spline_args),')'),collapse = " + ")
    gam_form = paste0('response ~ 1+ ',gam_form)
    if(length(binary_vars)>0) gam_form = paste0(gam_form,' + ', paste(binary_vars, collapse = " + "))
  }
  gam_form = stats::as.formula(gam_form)

  #model call
  command = list(
              gam_form,
              data =st$data[tetr$train_rows, ],
              family = indicator_family,
              weights = st$data[tetr$train_rows,get('data_weight')])
  command = append(command, sanitize_parameters(gam_params$args))
  mod = do.call(mgcv::gam, args = command)

  #create predictions
  output = predict(mod, st$data[tetr$test_rows,st$general_settings$covs, with = F], type = 'response')
  output = data.table(rid = tetr$test_rows, prediction = output)
  names(output) = c('rid', paste0(model_name,".",fold_col,".",fold_id))
  if(return_model_obj){
    return(list(output, mod, command))
  } else {
    return(list(output))
  }
}
