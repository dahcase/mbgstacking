#' Run a child model of the stacker using Sun Grid Engine
#'
#' @param stacking_folder path to folder where the stacker governer object is stored
#' @param st_name character. Name of the stacker
#' @param st_function character. The name of the model function to be run, not the function itself
#' @param rscript_path file path. Path to where script is saved on the machine.
#' @param model_name character. name of the model to be run
#' @param fold_col character vector. Denotes the name of the column designating the fold for crossval
#' @param fold_id Numeric. Designates the value in fold col that should be held out
#' @param return_model_obj logical. Denotes whether the function should return the earth object or just predictions.
#' @param slots The number of slots to request in the qsub
#' @param logfiles file path to where the output/error files should be stored
#' @param cleanup logical. Should the qsub delete the shell script at the end of the command?
#' @param package_location file path. Where is the mbgstacking package installed. Must be accessible by SGE
#' @param additional_qsub_options character. Additional options to pass to the qsub
#' @return List object with a data.table of predictions. If return_model_obj==T, the gam command and model object are returned as well
#' @import data.table
#' @importFrom stats predict
#' @export
#'

sge_run_child_model = function(stacking_folder = NULL, st_name = 'st', st_function = NULL, rscript_path = NULL, model_name = NULL, fold_col = NULL, fold_id = NULL,
                               return_model_obj = F, slots = 4, logfiles = '/dev/null/', cleanup = F, package_location = NULL, additional_qsub_options = NULL){

  #check to make sure stacking folder exists
  stopifnot(dir.exists(stacking_folder))
  #check to make sure rscript_path exists
  stopifnot(file.exists(rscript_path))
  #check to make sure package_location is legit and has mbgstacking
  stopifnot(dir.exists(package_location))
  #make sure model name isn't null
  stopifnot(!is.null(model_name))


  #load st, presumably saved before the call of this function
  #add a close slash if missing
  if(substring(stacking_folder, nchar(stacking_folder)-1, nchar(stacking_folder))!='/') stacking_folder = paste0(stacking_folder, '/')
  st_path = (paste0(stacking_folder, st_name, '.rds'))
  st = readRDS(st_path)

  #if so, make sure st_function is valid
  stopifnot(!is.null(get(st_function)))

  #build shell/R script
  shell_header = '#$ -S /bin/sh'
  library_call = paste0('library(\'mbgstacking\', lib.loc = ', package_location,')')
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

  save_model_name = paste(model_name, fold_col, fold_id, sep = '.')

  save_results = paste0('saveRDS(mod, ',save_model_name, '.rds)')

  #add quotes to the command
  the_commands = sapply(c(library_call,load_data,run_model, save_results), addQuotes)
  the_commands = paste0(' -e ',paste(the_commands, collapse =' -e '))

  #create the shell script
  qsub_shell = paste0(stacking_folder, save_model_name,'.sh')
  fileConn = file()
  writeLines(c(shell_header, paste0(rscript_path, ' ', the_commands)), fileConn)
  close(fileConn)

  #qsub
  #save logfiles ?
  if(!is.null(logfiles) & dir.exists(logfiles)) {
    qsub_logfiles = paste('-o', logfiles, '-e', logfiles)
  }else{
    qsub_logfiles = NULL
  }

  qsub_name = paste0('-N ', save_model_name)
  qsub_slots = paste0('-pe multi_slot ', slots)

  #get the parts
  qsub_call_parts = c('qsub', additional_qsub_options, qsub_logfiles, qsub_name, qsub_slots, qsub_shell)

  #remove null options
  qsub_call_parts = qsub_call_parts[!is.null(qsub_call_parts)]

  qsub = paste(qsub_call_parts, collapse = ' ')

  #launch the job and return the name for tracking
  system(qsub)
  return(qsub_name)

}
