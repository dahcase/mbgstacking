library(devtools)

for(ppp in c('data.table', 'raster', 'earth', 'mgcv', 'glmnet', 'parallel')){
  install.packages(ppp, lib = rpack)
}

#system('scl enable devtoolset-4 bash')
.libPaths('/share/geospatial/stacking_packages/')
rpack = '/share/geospatial/stacking_packages/'
devtools::install_github('dahcase/mbgstacking', lib = rpack)
