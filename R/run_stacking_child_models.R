#' Run child models for stacking
#'
#' Fits all models specified in the passed stacking object. M*C*K + M models are run where M is the number of initialized models,
#' C is the number of columns/iterations for crossfold validation and K is the number of folds.
#'
#' @param st stacker governer.
#' @import data.table
#' @export
#'
run_stacking_child_models = function(st){

  #build the grid to govern the mclapply call
  model_grid = data.table(expand.grid(
              model_name = names(st$models),
              fold_columns = st$general_settings$fold_cols,
              fold_ids = st$general_settings$fold_ids, stringsAsFactors = F),
              return_model_obj = F)

  #add the main model runs
  main_mods = data.table(model_name = names(st$models), return_model_obj = T)

  model_grid = rbind(model_grid,main_mods, fill = T)

  #initialize cluster
  clus = makeCluster(st$general_settings$cores)
  clusterEvalQ(clus, 'mbgstacking')

  #run the models
  stacking_models = parallel::parLapplyLB(clus, 1:nrow(model_grid),
                    function(x) get(paste0('fit_',get_model_type(st, model_grid[x,get('model_name')])))(
                      st = st,
                      model_name = model_grid[x,get('model_name')],
                      fold_col = model_grid[x,get('fold_columns')],
                      fold_id = model_grid[x,get('fold_ids')],
                      return_model_obj = model_grid[x,get('return_model_obj')]))
  stopCluster(clus)

    #set the names
    names(stacking_models) = paste(model_grid[,get('model_name')],
                                   model_grid[,get('fold_columns')],
                                   model_grid[,get('fold_ids')], sep = ".")
    #format the results to be in the full model cv model and the model objs

    #split predictions and model objects
    #model objects
    model_objs = sapply(stacking_models,'[',2)
    model_objs = model_objs[!sapply(model_objs, is.null)]

    #predictions
    preds = sapply(stacking_models,'[',1)
    #merge them all together
    preds = Reduce(function(...) merge(..., all = T), preds)

    #condense into full pred and cv pred
    #full preds
    preds = preds[,paste0(names(st$models),'_full_pred') := mget(paste0(names(st$models),'.NA.NA'))]

    #condense cv preds and create an object for return
    #select the required columns into a new dataset
    cv_preds = lapply(names(st$models), function(x) preds[,grep(paste0(x,'.sfold_'), names(preds), value = T), with =F ])
    cv_preds = lapply(cv_preds, function(x) rowMeans(x, na.rm =T))
    cv_preds = data.table(do.call(cbind, cv_preds))
    names(cv_preds) = paste0(names(st$models),'_cv_pred')

    #create return dataset with full predictions and cv predictions
    all_preds = cbind(preds[,paste0(names(st$models),'_full_pred'),with =F],cv_preds)
    #add rid
    all_preds = cbind(st$data[,'rid', with = F], all_preds)
    #fix model names
    names(model_objs) = names(st$models)

    #create return object
    ret_obj = list(all_preds, model_objs)
    names(ret_obj) = c('preds', 'model_objs')

    return(ret_obj)
}
