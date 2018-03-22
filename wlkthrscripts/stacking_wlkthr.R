load("/home/j/temp/dccasey/stackin/sssa_test_stacking.Rdata")
#rm(the_data) #make sure extract covariates works
rm(extract_covariates)
rm(emplogit)
rm(fit_gam)
.libPaths('/share/geospatial/rstudio_stack/')
library(data.table)
library(mbgstacking)

#get the names proper
the_data = copy(df)

#initialize child models
#em_def = init_earth(model_name = 'em_def')
#em_chg = init_earth(model_name = 'em_chg', arguments = list(degree = 3, nk = 45))
gm_def = init_gam(model_name = 'gm_def')
enet = init_penalized(model_name = 'enet',arguments = list(alpha = 1), emp_logit = F)
#brt = init_xgboost(model_name = 'brt')
#windows specific alternations
slots = 3

r_path = "SINGULARITYENV_MKL_NUM_THREADS=2 /share/local/singularity-2.4.2/bin/singularity exec --cleanenv  '/share/singularity-images/lbd/r_pkgs3.4.3gcc7mkl.simg' /usr/local/bin/R"
wf = paste0('/share/geospatial/dccasey/stacking/test/')
dir.create(wf, recursive = T)
logloc = paste0('/share/geospatial/dccasey/stacking/')
oot = paste0(logloc, 'output/')
eet = paste0(logloc, 'errors/')

sgeset =init_sge(working_folder = wf, r_path = r_path, repeat_iterations = 2, output_files= oot, error_files = eet,
                 project_name = 'proj_geospatial', other_options = NULL, slots_per_job = c(3,6),
                 package_location = '/share/geospatial/sing_stacking/')

#create the stacker governor
steak = init_stacker(list(enet, gm_def), #em_def, em_chg, enet, gm_def,
                   data = the_data,
                   indicator = indicator,
                   indicator_family = indicator_family,
                   fe_equation = all_fixed_effects,
                   covariate_layers = all_cov_layers,
                   centre_scale = T,
                   weight_col = NULL,
                   num_fold_cols = 0,
                   num_folds = 0,
                   cores = 1,
                   sge_parameters = sgeset)
st = steak
model_results = run_stacking_child_models(steak)

child_ras = make_all_children_rasters(st = steak, model_objects = model_results[[2]], time_points = c(2000, 2005, 2010, 2015))

#check whether raster extracts matches the data point
subdat = merge(steak$data, model_results[[1]], by = 'rid')
ras_extract = lapply(child_ras, function(x) raster::extract(x, as.data.frame(subdat[,.(longitude,latitude)] )))

#cbind results
ras_extract = do.call(cbind,ras_extract)
subdat = cbind(subdat, ras_extract)

#
# st = steak
# model_objects = model_results[[2]]
# time_points = c(2000, 2005, 2010, 2015)
#
# stopifnot(class(model_objects)=='list')
#
# #build a grid listing the years of analysis and submodels
# child_ras_grid = data.table::data.table(expand.grid(models = names(model_objects), time_scale = st$general_settings$time_scale, stringsAsFactors = F))
#
# #sort by model and then year
# data.table::setorderv(child_ras_grid, c('models', 'time_scale'))
#
# #make corrosponding time position variable (e.g. which raster it is likely to be)
# child_ras_grid = child_ras_grid[, ('time_position') := 1:length(st$general_settings$time_scale)]
#
# #keep only the time points we care about
# if(!is.null(time_points)) child_ras_grid = child_ras_grid[get('time_scale') %in% time_points,]
#
# #check to make sure ras_grid is greater than 0
# stopifnot(nrow(child_ras_grid)>0)
#
# x= 1
# model_obj = model_objects[[child_ras_grid[x,get('models')]]]
# model_settings = st$models[[child_ras_grid[x,get('models')]]]
# covs = lapply(st$covariate_layers, function(cl) fetch_covariate_layer(cl, child_ras_grid[x, get('time_position')]))
# cs_df = st$cs_df
# indicator_family = st$general_settings$indicator_family
#
# #convert rasters into a data table
# dm = data.table::data.table(raster::as.data.frame(raster::stack(covs), xy = T))
#
# #make a row id
# dm = dm[, ('row_id') := 1:.N]
#
# #create a template
# template = dm[, c('x','y','row_id'), with = F]
#
# #centre scale
# if(!is.null(cs_df)){
#   cs_dm = centreScale(dm[,names(covs), with = F], df = cs_df)
#   dm = cbind(dm[,c('x','y', 'row_id')], cs_dm)
# }
#
# #drop rows with NAs
# dm = na.omit(dm)
# good_rows = dm[,'row_id', with = F]
#
# (inherits(model_obj, 'xgb.Booster') | inherits(model_obj, 'raw'))
#
# if(class(model_obj)=='raw'){
#   model_obj = xgboost::xgb.load(model_obj)
# }
#
# dm = xgboost::xgb.DMatrix(data = as.matrix(dm[,st$general_settings$covs, with = F]))
#
# ret_obj = data.table::data.table(ret_obj = predict(model_obj, newdata = dm))
#
# pred_ras = predict(model_obj, newdata = dm)
# pred00 = predict(model_obj, newdata = as.matrix(steak$data[year==2000,st$general_settings$covs, with = F]))
# pred05 = predict(model_obj, newdata = as.matrix(steak$data[year==2005,st$general_settings$covs, with = F]))
#
#
