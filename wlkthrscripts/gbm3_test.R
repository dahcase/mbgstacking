library('gbm3')
library('dismo')
library(data.table)

#load some datarz
load("/home/j/temp/dccasey/stackin/sssa_test_stacking.Rdata")

the_data[, offset := log(N)]
all_fixed_effects = trimws(strsplit(all_fixed_effects, "+", fixed = T)[[1]])

#old

#new
ppp = gbm3::training_params(num_trees = 1000, interaction_depth = 3, bag_fraction = .7)
mod = gbm3::gbmt_fit(x = the_data[,..all_fixed_effects], y = the_data[, fever],
               distribution = gbm_dist('Poisson'), offset = the_data[,offset],
               cv_folds = 5,
               train_params = training_params(num_trees = 2000, interaction_depth = 3,
                                              min_num_obs_in_node = 10, shrinkage = 0.001, bag_fraction = 0.5, id =
                                                seq_len(nrow(the_data)), num_train = round(0.5 * nrow(the_data)), num_features = length(all_fixed_effects)-1))

mod2 = gbm3::gbmt_fit(x = the_data[,..all_fixed_effects], y = the_data[, fever],
                     distribution = gbm_dist('Poisson'), offset = the_data[,offset],
                     cv_folds = 5,
                     train_params = training_params(num_trees = 10000, interaction_depth = 3,
                                                    min_num_obs_in_node = 10, shrinkage = 0.001, bag_fraction = 0.5, id =
                                                      seq_len(nrow(the_data)), num_train = round(0.5 * nrow(the_data)), num_features = length(all_fixed_effects)-1))
blarg = predict(mod2, the_data[, ..all_fixed_effects], n.trees = gbmt_performance(mod2)[1])

#more treees
mod2 = gbm3::gbmt_fit(x = the_data[,..all_fixed_effects], y = the_data[, fever],
                      distribution = gbm_dist('Gaussian'), offset = the_data[,offset],
                      cv_folds = 5,
                      train_params = training_params(num_trees = 10000, interaction_depth = 3,
                                                     min_num_obs_in_node = 10, shrinkage = 0.001, bag_fraction = 0.5, id =
                                                       seq_len(nrow(the_data)), num_train = round(0.5 * nrow(the_data)), num_features = length(all_fixed_effects)-1))
blarg = predict(mod2, the_data[, ..all_fixed_effects], n.trees = gbmt_performance(mod2)[1])


#emplogiy
the_data[, empfev := mbgstacking::emplogit(fever, N)]
mod3 = gbm3::gbmt_fit(x = the_data[,..all_fixed_effects], y = the_data[, empfev],
                      distribution = gbm_dist('Gaussian'), offset = the_data[,offset], weights = as.numeric(the_data[,N]),
                      cv_folds = 10,
                      train_params = training_params(num_trees = 2, interaction_depth = 4,
                                                     min_num_obs_in_node = 10, shrinkage = 0.001, bag_fraction = 0.9, id =
                                                       seq_len(nrow(the_data)), num_train = round(0.5 * nrow(the_data)), num_features = length(all_fixed_effects)))
blarg3 = mbgstacking:::invlogit(predict(mod3, the_data[, ..all_fixed_effects], n.trees = gbmt_performance(mod3)[1]))
plot(blarg3, mbgstacking:::invlogit(the_data[,empfev]))

