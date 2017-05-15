#' Fit an Earth model
#'
#' Fit a multivariate regression spline model using earth::earth. See earth for more details.
#'
#' @param st stacker governer. Stacking governer object with an earth model initialized
#' @param model_name character vector. Name of the model to be run
#' @param fold_col character vector. Denotes the name of the column designating the fold for crossval
#' @param fold_id Numeric. Designates the value in fold col that should be held out
#' @param return_model_obj logical. Denotes whether the function should return the earth object or just predictions.
#' @return List object with a data.table of predictions. If return_model_obj==T, the earth command and model object are returned as well
#' @import data.table
#' @importFrom stats predict
#'
fit_earth = function(st, model_name = 'earth',fold_col = NULL, fold_id = NULL, return_model_obj = F){

  #subset the model parameters we need
  earth_params = st$models[[model_name]]
  indicator_family = st$general_settings$indicator_family
  indicator = st$general_settings$indicator

  #get test and train
  tetr = make_test_train(st$data, fold_col = fold_col, fold_id = fold_id)

  #set the response variable
  if(st$general_settings$indicator_family=="binomial"){
    response <- cbind(success = st$data[tetr$train_rows, get(indicator)], failure = st$data[tetr$train_rows, get('N')] - st$data[tetr$train_rows, get(indicator)])
  } else{
    response = st$data[tetr$train_rows,get(st$general_settings$indicator)]
  }

  #model call
  command = list(
              x = st$data[tetr$train_rows, st$general_settings$covs, with = F],
              y = response,
              weights = st$data[tetr$train_rows,get('data_weight')],
              glm = list(family = st$general_settings$indicator_family))
  command = append(command, sanitize_parameters(earth_params$args))
  mod = do.call(earth::earth, args = command)

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
