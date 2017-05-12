#' Fit penalized regression model
#'
#' Fit a glm or penalized glm using h2o::h2o.glm. See help(h2o.glm) for more details
#'
#' @param st stacker governer. Stacking governer object with a penalized model initialized
#' @param model_name character vector. Name of the model to be run
#' @param fold_col character vector. Denotes the name of the column designating the fold for crossval
#' @param fold_id Numeric. Designates the value in fold col that should be held out
#' @param return_model_obj logical. Denotes whether the function should return the earth object or just predictions.
#' @return List object with a data.table of predictions. If return_model_obj==T, the gam command and model object are returned as well
#' @import data.table
#'
fit_glmnet= function(st, model_name = 'pen',fold_col = NULL, fold_id = NULL, return_model_obj = F){

  #fetch params, copy data and fetch indicator settings
  pen_params = st$models[[model_name]]
  indicator_family = st$general_settings$indicator_family
  indicator = st$general_settings$indicator

  #set the response variable
  if(indicator_family == 'binomial'){
    #message('emplogit')
    #df[, y := emplogit(get(indicator), N)]
    if(pen_params$emp_logit){
      response_var = as.matrix(emplogit(st$data[,get(indicator)], st$data[,N]))
      indicator_family = 'gaussian'
    }else{
      #work with binomial in the traditional way
      response_var = cbind(failure = (st$data[,N]-st$data[,get(indicator)]), success = st$data[,get(indicator)])
    }
  } else{
    response_var = as.matrix(df[,get(indicator)])
  }

  #make set and train
  tetr = make_test_train(st$data, fold_col = fold_col, fold_id = fold_id)

  #glmnet can't handle cases where N = 1

  #make train design matrix
  dm = as.matrix(st$data[tetr$train_rows,st$general_settings$covs, with = F])
  colnames(dm) = st$general_settings$covs

  #make test dm
  newdata = as.matrix(st$data[tetr$test_rows,st$general_settings$covs, with = F])
  colnames(newdata) = st$general_settings$covs


  #model call
  command = list(
              x = dm,
              y = response_var[tetr$train_rows,],
              weights = st$data[tetr$train_rows,data_weight],
              family = indicator_family)
  command = append(command, sanitize_parameters(pen_params$args))

  #call cv.glm to get lambda
  cv_res = do.call(glmnet::cv.glmnet, args = command)

  #fit main model
  mod = do.call(glmnet::glmnet, args = command)

  #create predictions
  output = predict(mod, newx = newdata, s = cv_res$lambda.1se, type = 'link')
  if(indicator_family == 'binomial' | pen_params$emp_logit == T) output = invlogit(output)

  output = data.table(rid = tetr$test_rows, prediction = output)

  #fix names
  names(output) = c('rid', paste0(model_name,".",fold_col,".",fold_id))


  if(return_model_obj){
    return(list(output, mod, command))
  } else {
    return(list(output))
  }

}
