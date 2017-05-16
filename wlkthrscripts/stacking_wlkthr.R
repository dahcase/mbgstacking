load("C:/Users/dccasey/ownCloud/ihme_work/sssa_test_stacking.Rdata")
#rm(the_data) #make sure extract covariates works
rm(extract_covariates)
rm(emplogit)
library(data.table)
library(mbgstacking)

#get the names proper
the_data = copy(df)

#initialize child models
em_def = init_earth(model_name = 'em_def')
#em_chg = init_earth(model_name = 'em_chg', arguments = list(degree = 3, nk = 45))
gm_def = init_gam(model_name = 'gm_def')
enet = init_penalized(model_name = 'enet',arguments = list(alpha = 1), emp_logit = F)
brt = init_brt(model_name = 'brt')
#windows specific alternations
slots = 1

#create the stacker governor
steak = init_stacker(enet, gm_def, em_def,brt, #em_def, em_chg, enet, gm_def,
                   data = the_data,
                   indicator = indicator,
                   indicator_family = indicator_family,
                   fe_equation = all_fixed_effects,
                   covariate_layers = all_cov_layers,
                   centre_scale = T,
                   weight_col = NULL,
                   num_fold_cols = 5,
                   num_folds = 5,
                   cores = slots)

model_results = run_stacking_child_models(steak)

child_ras = make_all_children_rasters(st = steak, model_objects = model_results[[2]], time_points = c(2000, 2005, 2010, 2015))


#st = stk
#fold_col = NULL; fold_id = NULL; return_model_obj = F; sub_cores = 1
#model_name = 'brt'

