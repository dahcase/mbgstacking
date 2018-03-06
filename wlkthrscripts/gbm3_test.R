library('gbm3')
library('dismo')
library(data.table)

#load some datarz
load("C:/Users/dccasey/ownCloud/ihme_work/sssa_test_stacking.Rdata")

the_data[, offset := log(N)]
all_fixed_effects = trimws(strsplit(all_fixed_effects, "+", fixed = T)[[1]])

best_trees = rep.int(0, 6)
iter = 1
for(ss in c(100,50,25,10,5,1)){
  old = gbm.step(data.frame(the_data), gbm.x = all_fixed_effects, gbm.y = 'fever', offset = the_data[,offset],
                 family = 'poisson', max.trees = 10000, step.size = ss)
  best_trees[iter] <- old$gbm.call$best.trees
  iter = iter +1
}
oldpreds = predict(old, the_data, n.trees = old$gbm.call$best.trees )
