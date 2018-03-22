#' Run a child model of the stacker using Sun Grid Engine
#'
#' @param st stacker governer object
#' @param min_task integer. Starting task number. Generally refers to a row in st$model_grid
#' @param max_task integer. Ending task number. Generally refers to a row in st$model_grid
#' @return character string. Name of the sge launched
#' @import data.table
#' @export
#'
#
sge_run_child_model = function(st, min_task = 1, max_task = 1){

  #shorten the names of some things
  r_path = st$general_settings$sge_parameters$r_path
  package_location = st$general_settings$sge_parameters$package_location
  slots_per_job = st$general_settings$sge_parameters$child_model_slots
  sgecommand = st$general_settings$sge_parameters$sge_command
  working_folder = st$general_settings$sge_parameters$working_folder

  #check to make sure stacking folder exists
  stopifnot(dir.exists(working_folder))

  #input validation
  #check to make sure r_path exists
  #stopifnot(file.exists(r_path))

  #check to make sure package_location is legit and has mbgstacking
  #todo

  #build the R command
  add_lib_paths = paste0('.libPaths(', addQuotes_s(package_location), ')')
  library_call = paste0('library(mbgstacking , lib.loc = ', addQuotes_s(package_location),')')

  #get the sge variable
  get_task_id = "task_id = as.integer(Sys.getenv('SGE_TASK_ID'))"
  report_task = 'message(task_id)'
  st_path = paste0(working_folder,'st.rds')
  load_data = paste0('st = readRDS(',addQuotes_s(st_path),')')
  run_mod = 'run_model(st,task_id)'

  #add quotes to the command
  the_commands = sapply(c(get_task_id, report_task, add_lib_paths, library_call, load_data, run_mod), addQuotes_d)
  the_commands = paste0(' -e ',paste(the_commands, collapse =' -e '))

  #set r commands
  r_commands = paste0(r_path, ' ', the_commands)

  #create the shell script
  r_commands = paste('-b y', shQuote(paste('SINGULARITYENV_SGE_TASK_ID=$SGE_TASK_ID', r_commands)))

  #qsub
  qsub_name = paste0('-N child_models')
  qsub_slots = paste0('-pe multi_slot ', slots_per_job)

  #array
  stopifnot(max_task>=min_task)
  stopifnot(min_task > 0 )
  array_params = paste0('-t ', min_task, ':', max_task, " ")
  #get the parts
  qsub_call_parts = c('qsub -terse ', sgecommand, array_params,qsub_name, qsub_slots, r_commands)

  #remove null options
  qsub_call_parts = qsub_call_parts[!is.null(qsub_call_parts)]

  qsub = paste(qsub_call_parts, collapse = ' ')

  return(qsub)

}
