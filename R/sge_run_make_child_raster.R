#' Run a child model of the stacker using Sun Grid Engine
#'
#' @param st stacker governer object
#' @param model_obj_name character. The model object name
#' @param time_position numeric. relative position within the time scale that a raster should be generated
#' @return character string. Name of the sge launched
#' @import data.table
#' @export
#'
sge_run_make_child_raster = function(st, model_obj_name, time_position){

  r_path = st$general_settings$sge_parameters$r_path
  package_location = st$general_settings$sge_parameters$package_location
  slots_per_job = st$general_settings$sge_parameters$raster_slots
  sgecommand = st$general_settings$sge_parameters$sge_command
  working_folder = st$general_settings$sge_parameters$working_folder
  write_shell = st$general_settings$sge_parameters$write_shell
  stopifnot(dir.exists(working_folder))
  stopifnot(file.exists(paste0(working_folder, "st.rds")))
  stopifnot(file.exists(paste0(working_folder, "model_objects.rds")))
  #stopifnot(file.exists(r_path))
  shell_header = "#$ -S /bin/sh"
  library_call = paste0("library(mbgstacking , lib.loc = ",
                        addQuotes_s(package_location), ")")
  add_lib_paths = paste0(".libPaths(", addQuotes_s(package_location),
                         ")")
  st_path = paste0(working_folder, "st.rds")
  modobj_path = paste0(working_folder, "model_objects.rds")
  load_st = paste0("st = readRDS(", addQuotes_s(st_path), ")")
  load_model_obj = paste0("model_objects = readRDS(", addQuotes_s(modobj_path),
                          ")")
  mo = paste0("model_obj = model_objects[[", addQuotes_s(model_obj_name), "]]")
  ms = paste0("model_settings = st[['models']][[", addQuotes_s(model_obj_name),"]]")
  cv = paste0("covs = lapply(st[['covariate_layers']], function(cl) fetch_covariate_layer(cl,", time_position, "))")
  cs = "st[['cs_df']]"
  mf = "indicator_family = st[['general_settings']][['indicator_family']]"
  make_raster = paste0("ras = make_child_raster(", paste(mo,
                                                         ms, cv, cs, mf, sep = ","), ")")
  save_ras_name = paste(model_obj_name, time_position, sep = "_")
  save_results = paste0("saveRDS(ras, ", addQuotes_s(paste0(working_folder,
                                                            save_ras_name, ".rds")), ")")
  the_commands = sapply(c(library_call, add_lib_paths, load_st,
                          load_model_obj, make_raster, save_results), addQuotes_d)
  the_commands = paste0(" -e ", paste(the_commands, collapse = " -e "))
  r_commands = paste0(r_path, " ", the_commands)
  if (write_shell) {
    qsub_shell = paste0(working_folder, save_ras_name, ".sh")
    fileConn = file(qsub_shell)
    writeLines(c(shell_header, r_commands), fileConn)
    flush(fileConn)
    close(fileConn)
  }
  else {
    r_commands = qsub_shell = paste("-b y", shQuote(r_commands))
  }
  qsub_name = paste0("-N ", save_ras_name)
  qsub_slots = paste0("-pe multi_slot ", slots_per_job)
  qsub_call_parts = c("qsub", sgecommand, qsub_name, qsub_slots,
                      qsub_shell)
  qsub_call_parts = qsub_call_parts[!is.null(qsub_call_parts)]

  qsub = paste(qsub_call_parts, collapse = ' ')

  #launch the job and return the name for tracking
  #launch the job and return the name for tracking
  if(Sys.info()[1]=='Windows'){
    print(qsub)
    job_id = save_ras_name
  } else{
    job_id = system(qsub,intern =T)
    job_id = strsplit(job_id, " ")

    #keep only what is able to be numericed
    job_id = sapply(job_id, as.numeric)
    job_id = job_id[!is.na(job_id)]

  }

  return(job_id)
}
