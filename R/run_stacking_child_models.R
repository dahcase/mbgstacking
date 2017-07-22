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
  model_grid = make_model_grid(st, add_parents = T)

  #run the models
  #if sge is null, use mclapply
  if(is.null(st$general_settings$sge_parameters)){
    stacking_models = parallel::mclapply(1:nrow(model_grid),
                                        function(x) get(paste0('fit_',get_model_type(st, model_grid[x,get('model_name')])))(
                                          st = st,
                                          model_name = model_grid[x,get('model_name')],
                                          fold_col = model_grid[x,get('fold_columns')],
                                          fold_id = model_grid[x,get('fold_ids')],
                                          return_model_obj = model_grid[x,get('return_model_obj')]),
                                        mc.cores = st$general_settings$cores,mc.preschedule = F)

  } else{

    #save the stacker object
    saveRDS(st, file = paste0(st$general_settings$sge_parameters$working_folder, 'st.rds'))

    #Launch jobs on sge
    jobs = lapply(1:nrow(model_grid), function(x) sge_run_child_model(
                                        st = st,
                                        st_function = paste0('fit_',get_model_type(st, model_grid[x,get('model_name')])),
                                        model_name = model_grid[x,get('model_name')],
                                        fold_col = model_grid[x,get('fold_columns')],
                                        fold_id = model_grid[x,get('fold_ids')],
                                        return_model_obj = model_grid[x,get('return_model_obj')]))

    #launch a job held on all the sub jobs with sync
    sge_hold_via_sync(st, 'holder', jobs)

    #check to see if all the required files finished
    model_grid = model_grid[, ('files'):= paste(get('model_name'), get('fold_columns'), get('fold_ids'), sep = '_')]
    req_files = paste0(st$general_settings$sge_parameters$working_folder, model_grid[,get('files')], '.rds')

    #check which models didn't work.
    good_files = file.exists(req_files)

    #if everything IS NOT working
    if(!all(good_files)){
      repeat_iter = 0
      #alert the error log which files do not exist
      lapply(paste("These files do not exist: ", req_files[!good_files], jobs[!good_files]), message)

      #rerun models to see if it was cluster problems
      while(repeat_iter < st$general_settings$sge_parameters$repeat_iterations){
        new_model_grid = model_grid[!good_files,]

        #try rerunning
        message(paste('Rerunning', nrow(new_model_grid), 'models. Iter:', repeat_iter))

        jobs = lapply(1:nrow(new_model_grid), function(x) sge_run_child_model(
          st = st,
          st_function = paste0('fit_',get_model_type(st, new_model_grid[x,get('model_name')])),
          model_name = new_model_grid[x,get('model_name')],
          fold_col = new_model_grid[x,get('fold_columns')],
          fold_id = new_model_grid[x,get('fold_ids')],
          return_model_obj = new_model_grid[x,get('return_model_obj')]))

        #hold to see if things work
        sge_hold_via_sync(st, 'holder', jobs)

        #get the files that should exist
        req_files = paste0(st$general_settings$sge_parameters$working_folder, model_grid[,get('files')], '.rds')
        good_files = file.exists(req_files)

        if(all(good_files)){
          repeat_iter = st$general_settings$sge_parameters$repeat_iterations
        }

        repeat_iter = repeat_iter + 1
      }

      if(!all(good_files)){
        message('Not all models worked/finished')
        message(paste(req_files[!good_files]))
        stop()
      }

    }

    #read in the results
    stacking_models = lapply(req_files, readRDS)
  }

  #set the names
  names(stacking_models) = paste(model_grid[,get('model_name')],
                                 model_grid[,get('fold_columns')],
                                 model_grid[,get('fold_ids')], sep = "_")
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
  preds = setnames(preds, paste0(names(st$models),'.NA.NA'),paste0(names(st$models),'_full_pred'))
  #condense cv preds and create an object for return
  #select the required columns into a new dataset

  cv_preds = preds[,!grep('_full_pred', names(preds), fixed = T, value =T), with =F]
  cv_preds = cv_preds[, lapply(names(st$models), function(x) rowMeans(cv_preds[, grep(x, names(cv_preds), value = T), with = F], na.rm = T))]
  setnames(cv_preds,  paste0(names(st$models),'_cv_pred'))

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
