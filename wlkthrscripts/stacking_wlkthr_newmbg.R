load("C:/Users/dccasey/ownCloud/ihme_work/stackin/sssa_v9.Rdata")
#rm(the_data) #make sure extract covariates works
rm(extract_covariates)
rm(fit_earth)
rm(fit_gam)
rm(fit_glmnet)
rm(emplogit)
rm(addQuotes)
rm(fetch_covariate_layer)
library(data.table)
library(mbgstacking)

#get the names proper
the_data = copy(df)

#windows specific alternations
slots = 1
cores_to_use =1

#initialize some stackers
# earth_model = init_earth(model_name = 'earth')
# gam_model = init_gam(model_name = 'gam')
lasso_model = init_penalized('lasso', arguments = list(alpha = 1))
# ridge_model = init_penalized('ridge', arguments = list(alpha = 0))
# enet_model = init_penalized('ridge', arguments = list(alpha = .52))
# brt_model = init_brt()

sgeset =init_sge(working_folder = 'C:/Users/dccasey/Documents/illness_mapping/test_stacking', r_path = 'C:/Users/dccasey/ownCloud/ihme_work/stackin/sssa_v9.Rdata', output_files= '/dev/null', error_files = '/dev/null',
                  project_name = 'project_geospatial', other_options = NULL, slots_per_job = c(10,13) ,package_location = NULL)

#manually specify folds
df = df[, help_me := round(runif(nrow(df), 0, 1))]
df = df[, where_am_i := round(runif(nrow(df), 0, 1))]

all_fixed_effects = 'access + distrivers + evi'
all_cov_layers = all_cov_layers[c('access', 'distrivers', 'evi')]
#create the stacker governor
steak = init_stacker(lasso_model,#earth_model, gam_model, lasso_model,ridge_model, enet_model,brt_model, #em_def, em_chg, enet, gm_def,
                   data = df,
                   indicator = indicator,
                   indicator_family = indicator_family,
                   fe_equation = all_fixed_effects,
                   covariate_layers = all_cov_layers,
                   centre_scale = T,
                   time_var = 'year',
                   time_scale = 2000:2015,
                   weight_col = NULL,
                   num_fold_cols = c('help_me', 'where_am_i'),
                   num_folds = 5,
                   cores = 1,
                   sge_parameters = NULL)


model_results = run_stacking_child_models(steak)


#
# child_ras = make_all_children_rasters(st = steak, model_objects = model_results[[2]], time_points = c(2000, 2005, 2010, 2015))
#
# #check whether raster extracts matches the data point
# subdat = merge(steak$data, model_results[[1]], by = 'rid')
# ras_extract = lapply(child_ras, function(x) raster::extract(x, as.data.frame(subdat[,.(longitude,latitude)] )))
#
# #cbind results
# ras_extract = do.call(cbind,ras_extract)
# subdat = cbind(subdat, ras_extract)

xyt = copy(df[,c('latitude','longitude','year')])
covariate_list = all_cov_layers
time_var = 'year'
time_scale = 2000:2015

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

#create a column to interface with the covariate suffixes (namely period format)
if(!is.null(time_scale)){
  xyt[,('time_id') := match(get(time_var), time_scale)]
} else{
  xyt[,('time_id') := get(time_var)]
}

#make sure time id is not null
if(nrow(xyt[is.na(get('time_id')),])>0){
  stop('Not all rows have a valid time id. Please check time scale')
}

tv_cov_col_names = grep("(\\.[0-9]+)$",names(xyt), value =T)
idv = names(xyt)[!names(xyt) %in% tv_cov_col_names]

#remove trailing periods followed by numbers at the end of the string
tv_cov_col_names_uniq = unique(gsub('\\.[0-9]*$', '', tv_cov_col_names))

#organize into a list
tv_cov_col_names = lapply(tv_cov_col_names_uniq, function(x) paste0(x,'.',1:length(time_scale)))

#melt
xyt = data.table::melt(xyt, id.vars = idv,
                       measure = tv_cov_col_names, value.name = tv_cov_col_names_uniq, variable.factor =F)

if(length(tv_cov_col_names_uniq)==1){
  xyt = xyt[,('variable'):= sub(paste0(tv_cov_col_names_uniq,'.'), "", variable, fixed = T)]
}
xyt = xyt[, as.integer(get('variable'))]
