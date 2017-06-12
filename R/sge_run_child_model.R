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

  #check to make sure stacking folder exists
  stopifnot(dir.exists(working_folder))

  #make sure model name isn't null
  stopifnot(!is.null(model_name))

  #if so, make sure st_function is valid
  stopifnot(!is.null(get(st_function)))

  #shorten the names of some things
  rscript_path = st$general_settings$sge_parameters$rscript_path
  package_location = st$general_settings$sge_parameters$package_location
  slots_per_job = st$general_settings$sge_parameters$slots_per_job
  sgecommand = st$general_settings$sge_parameters$sge_command
  working_folder = st$general_settings$sge_parameters$working_folder

  #check to make sure rscript_path exists
  stopifnot(file.exists(rscript_path))

  #check to make sure package_location is legit and has mbgstacking
  #todo

  #build shell/R script
  shell_header = '#$ -S /bin/sh'
  library_call = paste0('library(\'mbgstacking\', lib.loc = ', package_location,')')
  st_path = paste0(working_folder,'st.rds')
  load_data = paste0('st = readRDS(',st_path,')')

  #set up model call
  #if fold col or fold id are null, change then to be character string 'NULL'
  if(is.null(fold_col)) fold_col = 'NULL'
  if(is.null(fold_id)) fold_id = 'NULL'

  model_call = paste0('(st', ', model_name = ','\'',model_name,'\'', ', fold_col = ', fold_col, ', fold_id = ', fold_id, ', return_model_obj =', return_model_obj,')')

  run_model = paste0('mod = ', st_function,model_call)

  #change back
  if(fold_col=='NULL') fold_col = NULL
  if(fold_id == 'NULL') fold_id = NULL

  save_model_name = paste(model_name, fold_col, fold_id, sep = '_')

  save_results = paste0('saveRDS(mod, ',save_model_name, '.rds)')

  #add quotes to the command
  the_commands = sapply(c(library_call,load_data,run_model, save_results), addQuotes)
  the_commands = paste0(' -e ',paste(the_commands, collapse =' -e '))

  #create the shell script
  qsub_shell = paste0(working_folder, save_model_name,'.sh')
  fileConn = file(qsub_shell)
  writeLines(c(shell_header, paste0(rscript_path, ' ', the_commands)), fileConn)
  flush(fileConn)
  close(fileConn)

  #qsub

  qsub_name = paste0('-N ', save_model_name)
  qsub_slots = paste0('-pe multi_slot ', slots_per_job)

  #get the parts
  qsub_call_parts = c('qsub', sgecommand, qsub_name, qsub_slots, qsub_shell)

  #remove null options
  qsub_call_parts = qsub_call_parts[!is.null(qsub_call_parts)]

  qsub = paste(qsub_call_parts, collapse = ' ')

  #launch the job and return the name for tracking
  ifelse(Sys.info()[1]=='Windows', print(qsub),system(qsub))
  return(qsub_name)

}
