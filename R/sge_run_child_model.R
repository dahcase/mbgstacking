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

sge_run_child_model = function(st, st_function = NULL, model_name = NULL, fold_col = NA, fold_id = NA,
                               return_model_obj = F){

  #shorten the names of some things
  r_path = st$general_settings$sge_parameters$r_path
  package_location = st$general_settings$sge_parameters$package_location
  slots_per_job = st$general_settings$sge_parameters$child_model_slots
  sgecommand = st$general_settings$sge_parameters$sge_command
  working_folder = st$general_settings$sge_parameters$working_folder
  write_shell = st$general_settings$sge_parameters$write_shell
  #check to make sure stacking folder exists
  stopifnot(dir.exists(working_folder))

  #make sure model name isn't null
  stopifnot(!is.null(model_name))

  #if so, make sure st_function is valid
  stopifnot(!is.null(get(st_function)))

  #check to make sure r_path exists
  stopifnot(file.exists(r_path))

  #check to make sure package_location is legit and has mbgstacking
  #todo

  #build shell/R script
  shell_header = '#$ -S /bin/sh'
  library_call = paste0('library(mbgstacking , lib.loc = ', addQuotes_s(package_location),')')
  add_lib_paths = paste0('.libPaths(', addQuotes_s(package_location), ')')

  st_path = paste0(working_folder,'st.rds')
  load_data = paste0('st = readRDS(',addQuotes_s(st_path),')')

  #set up model call
  #if fold col or fold id are null, change then to be character string 'NULL'
  if(is.na(fold_col) | is.null(fold_col)) fold_col = 'NA'
  if(is.na(fold_id) | is.null(fold_id)) fold_id = 'NA'

  model_call = paste0('(st', ', model_name = ',addQuotes_s(model_name), ', fold_col = ', addQuotes_s(fold_col), ', fold_id = ', fold_id, ', return_model_obj =', return_model_obj,')')

  run_model = paste0('mod = ', st_function,model_call)

  #change back
  if(fold_col=='NA') fold_col = NA
  if(fold_id == 'NA') fold_id = NA

  save_model_name = paste(model_name, fold_col, fold_id, sep = '_')

  save_results = paste0('saveRDS(mod, ',addQuotes_s(paste0(working_folder, save_model_name, '.rds')),')')

  #add quotes to the command
  the_commands = sapply(c(library_call,add_lib_paths, load_data,run_model, save_results), addQuotes_d)
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
    qsub_shell = paste0(working_folder, save_model_name,'.sh')
    fileConn = file(qsub_shell)
    writeLines(c(shell_header, r_commands), fileConn)
    flush(fileConn)
    close(fileConn)
  }else{
    r_commands =
      qsub_shell = paste('-b y', shQuote(r_commands))
  }

  #qsub
  qsub_name = paste0('-N ', save_model_name)
  qsub_slots = paste0('-pe multi_slot ', slots_per_job)

  #get the parts
  qsub_call_parts = c('qsub', sgecommand, qsub_name, qsub_slots, qsub_shell)

  #remove null options
  qsub_call_parts = qsub_call_parts[!is.null(qsub_call_parts)]

  qsub = paste(qsub_call_parts, collapse = ' ')

  #launch the job and return the name for tracking
  if(Sys.info()[1]=='Windows'){
    print(qsub)
    job_id = save_model_name
  } else{
    job_id = system(qsub,intern =T)
    job_id = strsplit(job_id, " ")

    #keep only what is able to be numericed
    job_id = sapply(job_id, as.numeric)
    job_id = job_id[!is.na(job_id)]

  }

  return(job_id)

}
