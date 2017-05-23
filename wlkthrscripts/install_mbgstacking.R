library(devtools)

for(ppp in c('data.table', 'raster', 'earth', 'mgcv', 'glmnet', 'parallel')){
  install.packages(ppp, lib = rpack)
}

.libPaths('/share/geospatial/stacking_packages/')
rpack = '/share/geospatial/stacking_packages/'

library('xgboost', lib = rpack)
devtools::install_github('dahcase/mbgstacking', lib = rpack)
