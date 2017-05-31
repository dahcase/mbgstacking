load("C:/Users/dccasey/ownCloud/ihme_work/stackin/sssa_v9.Rdata")
#rm(the_data) #make sure extract covariates works
rm(extract_covariates)
rm(emplogit)
library(data.table)
library(mbgstacking)

#get the names proper
the_data = copy(df)

#windows specific alternations
slots = 3
cores_to_use =3

#initialize some stackers
earth_model = init_earth(model_name = 'earth')
gam_model = init_gam(model_name = 'gam')
lasso_model = init_penalized('lasso', arguments = list(alpha = 1))
ridge_model = init_penalized('ridge', arguments = list(alpha = 0))
enet_model = init_penalized('ridge', arguments = list(alpha = .52))
brt_model = init_brt()

#create the stacker governor
steak = init_stacker(earth_model, gam_model, lasso_model,ridge_model, enet_model,brt_model, #em_def, em_chg, enet, gm_def,
                   data = df,
                   indicator = indicator,
                   indicator_family = indicator_family,
                   fe_equation = all_fixed_effects,
                   covariate_layers = all_cov_layers,
                   centre_scale = T,
                   weight_col = NULL,
                   num_fold_cols = 5,
                   num_folds = 5,
                   cores = cores_to_use)

#run it manually
#ARRRRRRRRRRRRRRGs
models = list(earth_model, gam_model, lasso_model,ridge_model, enet_model,brt_model)
data = df
indicator = indicator
indicator_family = indicator_family
fe_equation = all_fixed_effects
covariate_layers = all_cov_layers
centre_scale = T
weight_col = NULL
num_fold_cols = 5
num_folds = 5
cores = cores_to_use
time_var = 'year'
time_scale = c(2000:2015)

#init object
general_settings = list(indicator = indicator, indicator_family = indicator_family,
    weight_col = weight_col, fe_equation = fe_equation, cores = cores,
    covs = format_covariates(fe_equation))

govner = structure(list(general_settings = general_settings), class = 'stacker_governor')

#init dataset
govner$data = data.table::copy(data)

folds = make_stacking_folds(nrow(govner$data), numfolds = num_folds, numsets = num_fold_cols)
  govner$data = cbind(govner$data, folds)

cov_vals = extract_covariates(xyt = govner$data[,c('longitude','latitude',time_var), with = F],
                                covariate_list = covariate_layers,
                                centre_scale = centre_scale,
                                time_var = time_var,
                                time_scale = time_scale)



herp = copy(govner$data)
covv = cov_vals[[1]]

#covariates args
xyt = govner$data[,c('longitude','latitude',time_var), with = F]
covariate_list = covariate_layers
centre_scale = centre_scale
time_var = time_var
time_scale = time_scale

xyt = copy(xyt)

#check for names
check_names(xyt, c('latitude','longitude',time_var))

#if a list, condense to a brick-- outcome should be the same
if(class(covariate_list) == 'list'){
   covariate_list = raster::brick(covariate_list)
}

#subset to the three required columns
xyt = xyt[, c('latitude','longitude',time_var), with = F]

#create a row id
xyt = xyt[,('rid') := 1:nrow(xyt)]

  #create a variable

#extract the covariates
cov_values = raster::extract(covariate_list, xyt[, c('longitude', 'latitude'), with = F])

xyt = cbind(xyt, cov_values)

if(!is.null(time_scale)){
    xyt[,('time_id') := match(get(time_var), time_scale)]
  } else{
    xyt[,('time_id') := get(time_var)]
  }

tv_cov_col_names = grep("(\\.[0-9]+)$",names(xyt), value =T)
  idv = names(xyt)[!names(xyt) %in% tv_cov_col_names]

  #remove trailing periods followed by numbers at the end of the string
  tv_cov_col_names_uniq = unique(gsub('\\.[0-9]*$', '', tv_cov_col_names))

  #organize into a list
  tv_cov_col_names = lapply(tv_cov_col_names_uniq, function(x) grep(x, tv_cov_col_names, value = T))

  #melt
  xyt = data.table::melt(xyt, id.vars = idv,
            measure = tv_cov_col_names, value.name = tv_cov_col_names_uniq, variable.factor =F)

  #keep only rows where there is data time/covariate time agreement
  xyt = xyt[get('time_id') == get('variable') ,]

#add to dataset
  govner$data = cbind(govner$data, cov_vals[[1]])
  if(centre_scale) govner$cs_df = cov_vals[[2]]

  #omit missing rows and make row ids
  govner$data = na.omit(govner$data, cols = govner$general_settings$covs)
  govner$data[,('rid') := 1:nrow(govner$data)]

  #add to general settings
  govner$general_settings$fold_cols = names(folds)
  govner$general_settings$fold_ids = 1:num_folds

  #add time settings
  govner$general_settings$time_var = time_var
  govner$general_settings$time_scale = time_scale

  #set up the weight column
  if(!is.null(weight_col)){
    govner$data[,('data_weight') := govner$data[,get(weight_col)]]
  } else {
    govner$data[,('data_weight') := 1]
  }

  ##add the objects
  govner$models = list()
  for(obj in models){
    govner$models[[obj[['model_name']]]] <- obj
  }

  ##add covariate layer pointers
  govner$covariate_layers = covariate_layers



model_results = run_stacking_child_models(steak)

child_ras = make_all_children_rasters(st = steak, model_objects = model_results[[2]], time_points = c(2000, 2005, 2010, 2015))

#check whether raster extracts matches the data point
subdat = merge(steak$data, model_results[[1]], by = 'rid')
ras_extract = lapply(child_ras, function(x) raster::extract(x, as.data.frame(subdat[,.(longitude,latitude)] )))

#cbind results
ras_extract = do.call(cbind,ras_extract)
subdat = cbind(subdat, ras_extract)


