#' A little wrapper to run models via SGE
#' @param st stacking governer
#' @param task_id integer. refers to row in st$model_grid
#' @import data.table
#' @export
#'
run_model = function(st, task_id){
  working_folder = st$general_settings$sge_parameters$working_folder

  task_info = st$model_grid[task_id,]

  #run the model
  themod = get(task_info$model_type)
  mod = themod(st = st,
               model_name = task_info$model_name,
               fold_col = task_info$fold_columns,
               fold_id = task_info$fold_ids,
               return_model_obj = task_info$return_model_obj)

  save_model_name = paste(task_info$model_name,  task_info$fold_columns, task_info$fold_ids, sep = '_')

  saveRDS(mod, file.path(working_folder, save_model_name, '.rds'))

}
