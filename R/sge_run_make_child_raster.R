#' Run a child model of the stacker using Sun Grid Engine
#'
#' @param st stacker governer object
#' @param st_function character. The name of the model function to be run, not the function itself
#' @param model_name character. name of the model to be run
#' @param fold_col character vector. Denotes the name of the column designating the fold for crossval
#' @param fold_id Numeric. Designates the value in fold col that should be held out
#' @param return_model_obj logical. Denotes whether the function should return the earth object or just predictions.
#' @return character string. Name of the sge launched
#' @import data.table
#' @export
#'

#make_child_raster = function(model_obj, model_settings = NULL,  covs, cs_df = NULL, indicator_family = 'binomial')
# raster_objects = parallel::mclapply(1:nrow(child_ras_grid),
#                                     function(x) make_child_raster(
#                                       model_obj = model_objects[[child_ras_grid[x,get('models')]]],
#                                       model_settings = st$models[[child_ras_grid[x,get('models')]]],
#                                       covs = lapply(st$covariate_layers, function(cl) fetch_covariate_layer(cl, child_ras_grid[x, get('time_position')])),
#                                       cs_df = st$cs_df,
#                                       indicator_family = st$general_settings$indicator_family), mc.cores = st$general_settings$cores, mc.preschedule = F)

sge_run_make_child_raster = function(st, model_obj_name, time_position){

  #shorten the names of some things
  r_path = st$general_settings$sge_parameters$r_path
  package_location = st$general_settings$sge_parameters$package_location
  slots_per_job = st$general_settings$sge_parameters$slots_per_job
  sgecommand = st$general_settings$sge_parameters$sge_command
  working_folder = st$general_settings$sge_parameters$working_folder
  write_shell = st$general_settings$sge_parameters$write_shell

  #check to make sure stacking folder exists
  stopifnot(dir.exists(working_folder))

  #make sure the st and model objects things live in the working folder
  stopifnot(file.exists(paste0(working_folder,'st.rds')))
  stopifnot(file.exists(paste0(working_folder,'model_objects.rds')))

  #check to make sure r_path exists
  stopifnot(file.exists(r_path))

  #check to make sure package_location is legit and has mbgstacking
  #todo

  #build shell/R script
  shell_header = '#$ -S /bin/sh'
  library_call = paste0('library(mbgstacking , lib.loc = ', addQuotes_s(package_location),')')
  add_lib_paths = paste0('.libPaths(', addQuotes_s(package_location), ')')

  #load st and model objects
  st_path = paste0(working_folder,'st.rds')
  modobj_path = paste0(working_folder,'model_objects.rds')

  load_st = paste0('st = readRDS(',addQuotes_s(st_path),')')
  load_model_obj = paste0('model_objects = readRDS(',addQuotes_s(modobj_path),')')

  #make the raster
  mo = paste0('model_obj = model_objects[[',addQuotes_s(model_obj_name),']]')
  ms = paste0('model_settings = st$models[[', addQuotes_s(model_obj_name), ']]')
  cv = paste0('covs = lapply(st$covariate_layers, function(cl) fetch_covariate_layer(cl,', time_position,'))')
  cs = 'st$cs_df'
  mf = 'indicator_family = st$general_settings$indicator_family'
  make_raster = paste0('ras = make_child_raster(', paste(mo, ms, cv, cs,mf, sep = ','), ')')

  save_ras_name = paste(model_obj_name,time_position, sep = '_')

  save_results = paste0('saveRDS(ras, ',addQuotes_s(paste0(working_folder, save_ras_name, '.rds')),')')

  #add quotes to the command
  the_commands = sapply(c(library_call,add_lib_paths, load_st, load_model_obj, make_raster, save_results), addQuotes_d)
  the_commands = paste0(' -e ',paste(the_commands, collapse =' -e '))

  #write lines to activate the environment if called for
  # if(!is.null(st$general_settings$sge_parameters$conda_activate)){
  #   activate_conda = paste(st$general_settings$sge_parameters$conda_activate, st$general_settings$sge_parameters$conda_env)
  # }else{
  #   activate_conda = ""
  # }

  #set r commands
  r_commands = paste0(r_path, ' ', the_commands)

  #create the shell script
  if(write_shell){
    qsub_shell = paste0(working_folder, save_ras_name,'.sh')
    fileConn = file(qsub_shell)
    writeLines(c(shell_header, r_commands), fileConn)
    flush(fileConn)
    close(fileConn)
  }else{
    r_commands =
      qsub_shell = paste('-b y', shQuote(r_commands))
  }

  #qsub
  qsub_name = paste0('-N ', save_ras_name)
  qsub_slots = paste0('-pe multi_slot ', slots_per_job)

  #get the parts
  qsub_call_parts = c('qsub', sgecommand, qsub_name, qsub_slots, qsub_shell)

  #remove null options
  qsub_call_parts = qsub_call_parts[!is.null(qsub_call_parts)]

  qsub = paste(qsub_call_parts, collapse = ' ')

  #launch the job and return the name for tracking
  #launch the job and return the name for tracking
  if(Sys.info()[1]=='Windows'){
    print(qsub)
    job_id = save_ras_name
  } else{
    job_id = system(qsub,intern =T)
    job_id = strsplit(blerg, " ")

    #keep only what is able to be numericed
    job_id = sapply(job_id, as.numeric)
    job_id = job_id[!is.na(job_id)]

  }

  return(job_id)
  #return(qsub)
}
