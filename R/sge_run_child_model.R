#' Run a child model of the stacker using Sun Grid Engine
#'
#' @param stacking_folder path to folder where the stacker governer object is stored
#' @param st_name character. Name of the stacker
#' @param st_function character. Name of the model function to be run
#' @param rscript_path file path. Path to where script is saved on the machine.
#' @param fold_col character vector. Denotes the name of the column designating the fold for crossval
#' @param fold_id Numeric. Designates the value in fold col that should be held out
#' @param return_model_obj logical. Denotes whether the function should return the earth object or just predictions.
#' @param slots The number of slots to request in the qsub
#' @param logfiles file path to where the output/error files should be stored
#' @param cleanup logical. Should the qsub delete the shell script at the end of the command?
#' @return List object with a data.table of predictions. If return_model_obj==T, the gam command and model object are returned as well
#' @import data.table
#' @importFrom stats predict
#' @export
#'

sge_run_child_model = function(stacking_folder = NULL, st_name = 'st', st_function = NULL, rscript_path = NULL, fold_col = NULL, fold_id = NULL,
                               return_model_obj = NULL, slots = 4, logfiles = '/dev/null/', cleanup = F){

  #check to make sure stacking folder exists
  #check to make sure rscript_path exists
  #if so, make sure st_function is valid

  #build shell/R script

  #write to disk

  #qsub



}






mclapply(1:nrow(model_grid),
         function(x) get(paste0('fit_',get_model_type(st, model_grid[x,get('model_name')])))(
           st = st,
           model_name = model_grid[x,get('model_name')],
           fold_col = model_grid[x,get('fold_columns')],
           fold_id = model_grid[x,get('fold_ids')],
           return_model_obj = model_grid[x,get('return_model_obj')]),
         mc.cores = st$general_settings$cores,mc.preschedule = F)

#/share/geospatial/stacking_conda/envs/stacking/bin/Rscript

#args:
#1) folder where st object is saved and the shell script will be saved
#2) jobname


#save st

#build the command to pass through Rscripts -e command
#read task number
#load task governer
#run model
#save result

#write shell script



#launch job
