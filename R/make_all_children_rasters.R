#' Make rasters from the child model objects
#'
#' Using model fits from run_stacking_child_models, creates rasters
#' @param st stacker governer.
#' @param model_objects a named list of model objects
#' @param time_points numeric vector. the name/identifier/subset to be predicted-- on the scale of time_var
#' @import data.table
#' @export
#'
make_all_children_rasters = function(st, model_objects, time_points = NULL){

  #make sure model_objects is being passed as a list
  stopifnot(class(model_objects)=='list')

  #build a grid listing the years of analysis and submodels
  child_ras_grid = data.table::data.table(expand.grid(models = names(model_objects), time_scale = st$general_settings$time_scale, stringsAsFactors = F))

  #sort by model and then year
  data.table::setorderv(child_ras_grid, c('models', 'time_scale'))

  #make corrosponding time position variable (e.g. which raster it is likely to be)
  child_ras_grid = child_ras_grid[, ('time_position') := 1:length(st$general_settings$time_scale)]

  #keep only the time points we care about
  if(!is.null(time_points)) child_ras_grid = child_ras_grid[get('time_scale') %in% time_points,]

  #check to make sure ras_grid is greater than 0
  stopifnot(nrow(child_ras_grid)>0)

  #make rasters from the selected child models-- this probably won't scale super well
  if(is.null(st$general_settings$sge_parameters)){
    raster_objects = parallel::mclapply(1:nrow(child_ras_grid),
                              function(x) make_child_raster(
                                model_obj = model_objects[[child_ras_grid[x,get('models')]]],
                                model_settings = st$models[[child_ras_grid[x,get('models')]]],
                                covs = lapply(st$covariate_layers, function(cl) fetch_covariate_layer(cl, child_ras_grid[x, get('time_position')])),
                                cs_df = st$cs_df,
                                indicator_family = st$general_settings$indicator_family), mc.cores = st$general_settings$cores, mc.preschedule = F)
  }else{
    #save model_objects to working folder
    saveRDS(model_objects, paste0(st$general_settings$sge_parameters$working_folder, 'model_objects.rds'))

    #submit qsubs
    jobs = lapply(1:nrow(child_ras_grid), function(x) sge_run_make_child_raster(st = st,
                                                                                model_obj_name = child_ras_grid[x,get('models')],
                                                                                time_position = child_ras_grid[x,get('time_position')]))
    #submit hold job
    sge_hold_via_sync(st, 'holder', jobs)

    #load results
    child_ras_grid = child_ras_grid[, ('files'):= paste(get('models'), get('time_position'), sep = '_')]
    req_files = paste0(st$general_settings$sge_parameters$working_folder, child_ras_grid[,get('files')], '.rds')

    #check which models didn't work.
    good_files = file.exists(req_files)

    #if everything IS NOT working
    if(!all(good_files)){
      repeat_iter = 0

      #alert the error log which files do not exist
      lapply(paste("These files do not exist: ", req_files[!good_files], jobs[!good_files]), message)

      #rerun models to see if it was cluster problems
      while(repeat_iter < st$general_settings$sge_parameters$repeat_iterations){
        new_model_grid = child_ras_grid[!good_files,]

        #try rerunning
        message(paste('Rerunning', nrow(new_model_grid), 'models. Iter:', repeat_iter))

        #submit qsubs
        jobs = lapply(1:nrow(new_model_grid), function(x) sge_run_make_child_raster(st = st,
                                                                                    model_obj_name = new_model_grid[x,get('models')],
                                                                                    time_position = new_model_grid[x,get('time_position')]))

        #hold to see if things work
        sge_hold_via_sync(st, paste0('holder_repeat', repeat_iter) , jobs)

        #get the files that should exist
        req_files = paste0(st$general_settings$sge_parameters$working_folder, child_ras_grid[,get('files')], '.rds')
        good_files = file.exists(req_files)

        if(all(good_files)){
          repeat_iter = st$general_settings$sge_parameters$repeat_iterations
        }

        repeat_iter = repeat_iter + 1
      }

      #if after a few iterations, things haven't finished. call it and complain
      if(!all(good_files)){
        message('Not all models worked/finished')
        message(paste(req_files[!good_files]))
        stop()
      }

    }




    #read in the results
    raster_objects = lapply(req_files, readRDS)
  }

  #create raster bricks
  #make row ids on the child ras grid
  child_ras_grid = child_ras_grid[,('rid') := 1:.N]
  raster_objects = setNames(lapply(unique(child_ras_grid[,get('models')]), #for each model
                                          function(x) raster::brick(raster_objects[child_ras_grid[get('models') == x, get('rid')]])),
                            unique(child_ras_grid[,get('models')]))
  return(raster_objects)
}

