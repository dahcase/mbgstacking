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

#windows specific alternations
slots = 1

#create the stacker governor
stk = init_stacker(enet, gm_def, em_def,#em_def, em_chg, enet, gm_def,
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

herp = run_stacking_child_models(stk)

