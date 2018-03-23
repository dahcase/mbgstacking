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
  #run the models
  #if sge is null, use mclapply
  if(is.null(st$general_settings$sge_parameters)){

    stacking_models = parallel::mclapply(1:nrow(st$model_grid),
                                        function(x) get(st$model_grid[x,get('model_type')])(
                                          st = st,
                                          model_name = st$model_grid[x,get('model_name')],
                                          fold_col = st$model_grid[x,get('fold_columns')],
                                          fold_id = st$model_grid[x,get('fold_ids')],
                                          return_model_obj = st$model_grid[x,get('return_model_obj')]),
                                        mc.cores = st$general_settings$cores,mc.preschedule = F)

  } else{

    #save the stacker object
    saveRDS(st, file = paste0(st$general_settings$sge_parameters$working_folder, 'st.rds'))

    #Launch jobs on sge
    joblaunch = sge_run_child_model(st, 1, nrow(st$model_grid))
    jobs = system(joblaunch,intern = T)

    #parse the array job
    jobs = substr(jobs, 1, regexec('.', jobs, fixed = T)[[1]][1]-1)

    #launch a job held on all the sub jobs with sync
    sge_hold_via_sync(st, 'holder', jobs)

    #check to see if all the required files finished
    subcheck = check_submodels(st)
    good_files = subcheck[[1]]

    #if everything IS NOT working
    if(!all(good_files)){
      repeat_iter = 0
      #alert the error log which files do not exist
      for(ppp in paste("These jobs do not exist: ", paste0(jobs, which(!good_files)))){
        message(ppp)
      }

      #rerun models to see if it was cluster problems
      while(repeat_iter < st$general_settings$sge_parameters$repeat_iterations){

        #try rerunning
        message(paste('Rerunning', length(good_files), 'models. Iter:', repeat_iter))

        #launch the guys
        jobs = lapply(which(!good_files), function(x) sge_run_child_model(st, x,x))
        jobs = unlist(lapply(jobs, function(x) substr(x, 1, regexec('.', x, fixed = T)[[1]][1]-1)))

        #hold to see if things work
        sge_hold_via_sync(st, 'holder', jobs)

        #get the files that should exist
        subcheck = check_submodels(st)
        good_files= subcheck[[1]]

        if(all(good_files)){
          repeat_iter = st$general_settings$sge_parameters$repeat_iterations
        }

        repeat_iter = repeat_iter + 1
      }

      if(!all(good_files)){
        message('Not all models worked/finished')

        #list broken files
        for(fff in subcheck[[2]][subcheck[[1]]]){
          message(paste('Missing:',fff))
        }

        stop('Not all models worked/finished')
      }

    }

    #read in the results
    stacking_models = lapply(subcheck[[2]], readRDS)
  }

  return(compile_submodels(st, stacking_models))

}
