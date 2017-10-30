#' Initialize SGE parameters
#'
#' Creates an object designed to be passed to init_stacker that describes how the internal machinery should handle .
#'
#' @param working_folder file path. Location accessible to the computing cluster where scratch files can be saved
#' @param r_path file path. Full path to R
#' @param output_files character string. Location where the output (.o) files are saved
#' @param error_files character string. Location where the error (.e) files are save
#' @param project_name character string. Cluster project
#' @param other_options character string. Character string of additional options to pass to qsub
#' @param slots_per_job numeric. Denotes the number of slots to be requested by each submodel. If length>1, first item is passed to child models and second to raster creation
#' @param package_location character string. Denotes the location where mbgstacking is installed (to be passed to the qsubs)
#' @param repeat_iterations numeric: How many times should jobs be relaunched if they don't work the first time.
#' @param conda_activate file path. Denotes the location where a conda environment activate option is. Not implemented.
#' @param conda_env file path. File path to the name of the conda environment. Not implemented.
#' @param write_shell logical. Should the qsub be run via writing shell scripts. If F, qsubs are lanunched via the "-b y" flag
#' @return List of lists containing the input parameters to be passed to the stacker
#' @export
#'
init_sge = function(working_folder, r_path, output_files = NULL, error_files = NULL, project_name = NULL, other_options = NULL, slots_per_job = c(3,5), package_location = NULL, repeat_iterations = 0, conda_activate = NULL, conda_env = NULL, write_shell = F){

  output <- error <- project <- NULL

  #standardize file paths
  if(substring(working_folder, nchar(working_folder), nchar(working_folder))!='/') working_folder = paste0(working_folder, '/')

  #build the sge stuff
  if(!is.null(output_files)) output = paste('-o',output_files)
  if(!is.null(error_files)) error = paste('-e', error_files)
  if(!is.null(project_name)) project = paste('-P', project_name)

  #combine
  sge_command = c(output, error, project, other_options)
  sge_command = sge_command[!is.null(sge_command)]
  sge_command = paste(sge_command, collapse = " ")

  #slotting
  if(length(slots_per_job)>1){
    child_model_slots = slots_per_job[1]
    raster_slots = slots_per_job[2]
  } else{
    child_model_slots <- raster_slots <- slots_per_job
  }

  return(list(working_folder = working_folder, r_path = r_path, sge_command = sge_command, child_model_slots = child_model_slots, raster_slots = raster_slots,
              package_location = package_location, repeat_iterations = repeat_iterations, conda_activate = conda_activate, conda_env = conda_env, write_shell = write_shell))

}
