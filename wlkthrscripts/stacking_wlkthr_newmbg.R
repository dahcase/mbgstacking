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

sgeset = init_sge(working_folder = 'J:/temp/dccasey/', rscript_path = NULL, output_files= '/dev/null', error_files = '/dev/null',
                  project_name = 'project_geospatial', other_options = NULL, slots_per_job = 3 ,package_location = NULL)

#create the stacker governor
steak = init_stacker(earth_model, gam_model, lasso_model,ridge_model, enet_model,brt_model, #em_def, em_chg, enet, gm_def,
                   data = df,
                   indicator = indicator,
                   indicator_family = indicator_family,
                   fe_equation = all_fixed_effects,
                   covariate_layers = all_cov_layers,
                   centre_scale = T,
                   time_var = 'year',
                   time_scale = 2000:2016,
                   weight_col = NULL,
                   num_fold_cols = 5,
                   num_folds = 5,
                   cores = cores_to_use,
                   sge_parameters = sgeset)



#test out sge_run_child_model
working_folder = 'J:/temp/dccasey/'

#implict step
saveRDS(steak, paste0(working_folder,'steak.rds'))


st_name = 'steak'
st_function = 'fit_gam'
model_name = 'gam'
fold_col = NULL
fold_id = NULL
return_model_obj = F




# model_results = run_stacking_child_models(steak)
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


