#Daniel Casey
#Created: 2-24-2017
#Purpose: Functions involved in fitting earth (multivariate adaptive regression spline) models
#------------------------------------------------------------------------------#
fit_earth = function(st, model_name = 'earth',fold_col = NULL, fold_id = NULL, return_model_obj = F){

  #subset the model parameters we need
  earth_params = st$models[[model_name]]

  #get test and train
  tetr = make_test_train(st$data, fold_col = fold_col, fold_id = fold_id)

  #set the response variable
  if(st$general_settings$indicator_family=="binomial"){
    response <- cbind(success = st$data[tetr$train_rows, get(indicator)], failure = st$data[tetr$train_rows, N] - st$data[tetr$train_rows, get(indicator)])
  } else{
    response = st$data[tetr$train_rows,get(st$general_settings$indicator)]
  }

  #model call
  command = list(
              x = st$data[tetr$train_rows, st$general_settings$covs, with = F],
              y = response,
              weights = st$data[tetr$train_rows,data_weight],
              glm = list(family = st$general_settings$indicator_family))
  command = append(command, sanitize_parameters(earth_params$args))
  mod = do.call(earth, args = command)

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
