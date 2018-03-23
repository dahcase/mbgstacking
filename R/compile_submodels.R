#' Compile submodels
#' Turns a list of results created by various fit* functions into a two item list. Item 1 is a data.table of full and cv predictions. Item 2 is a list of model objects
#' @param st stacking governer
#' @param stacking_models list. List of results from the various fit* functions
#' @import data.table
#' @export
#'
compile_submodels = function(st, stacking_models){

  #check list is in proper format

  #seperate out model objects
  model_objs = sapply(stacking_models,'[',2)
  model_objs = model_objs[!sapply(model_objs, is.null)]

  #get the predictions
  #combine the predictions
  preds = st$data[,list(rid)]

  for(ppp in sapply(stacking_models,'[',1)){
    preds[ppp[,rid], (names(ppp)[2]) := ppp[[2]]]
  }

  #condense into full preds and cv preds
  preds = setnames(preds, paste0(names(st$models),'.NA.NA'),paste0(names(st$models),'_full_pred'))
  cv_preds = preds[,!grep('_full_pred', names(preds), fixed = T, value =T), with =F]
  cv_preds = cv_preds[, lapply(names(st$models), function(x) rowMeans(cv_preds[, grep(x, names(cv_preds), value = T), with = F], na.rm = T))]
  setnames(cv_preds,  paste0(names(st$models),'_cv_pred'))

  #check for holdouts
  if(nrow(cv_preds)>0){
    all_preds = cbind(preds[,paste0(names(st$models),'_full_pred'),with =F],cv_preds)
    #add rid
    all_preds = cbind(st$data[,'rid', with = F], all_preds)
  }else{
    all_preds = preds
  }
  #fix model names
  names(model_objs) = names(st$models)

  #create return object
  ret_obj = list(all_preds, model_objs)
  names(ret_obj) = c('preds', 'model_objs')

  return(ret_obj)
}
