fit_penalized = function(st, model_name = 'pen',fold_col = NULL, fold_id = NULL, return_model_obj = F){
  #load libs
  library(h2o, lib.loc = st$general_settings$lib.loc)
  pen_params = st$models[[model_name]]


  #if h2o isn't running, break
  h2o.init(startH2O=FALSE)
  df = copy(st$data)

  indicator_family = st$general_settings$indicator_family

  #set the response variable
  if(indicator_family == 'binomial' | indicator_family == 'poisson'){
    #message('emplogit')
    #df[, y := emplogit(get(indicator), N)]
    if(pen_params$emp_logit){
      df[, response := emplogit(get(indicator), N)]
      indicator_family = 'gaussian'
      offset_column = NULL
    }else{
      df[, response := round(get(indicator))]
      df[, offset := log(N)]
      offset_column = 'offset'
      indicator_family = 'poisson'
    }
  } else{
    df[,response:= get(indicator)]
    offset_column = NULL
  }

  #make set and train
  tetr = make_test_train(st$data, fold_col = fold_col, fold_id = fold_id)

  df.h2o = as.h2o(df[tetr$train_rows, ])

  #model call
  command = list(
              x = st$general_settings$covs,
              y = 'response',
              training_frame = df.h2o,
              weights = 'data_weight',
              family = indicator_family,
              offset_column = offset_column,
              lambda_search = pen_params$lambda_search)
  command = append(command, sanitize_parameters(pen_params$args))
  mod = do.call(h2o.glm, args = command)


}
