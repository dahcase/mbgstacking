#' A little wrapper to check the status of submodels
#' @param st stacking governer
#' @import data.table
#' @export
#'
check_submodels = function(st){
  file_stems = paste(st$model_grid$model_name, st$model_grid$fold_columns, st$model_grid$fold_ids, sep = '_')
  req_files = paste0(st$general_settings$sge_parameters$working_folder, file_stems, '.rds')
  good_files = file.exists(req_files)

  return(list(good_files, req_files))
}
