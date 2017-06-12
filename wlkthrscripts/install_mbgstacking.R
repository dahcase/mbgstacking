library(devtools)
rpack = '/share/geospatial/stacking_packages2/'
for(ppp in c('data.table', 'raster', 'earth', 'mgcv', 'glmnet', 'parallel')){
  install.packages(ppp, lib = rpack)
}

#system('scl enable devtoolset-4 bash')
rpack = '/share/geospatial/stacking_packages/'
.libPaths('/share/geospatial/stacking_packages/')
devtools::install_github('dahcase/mbgstacking', lib = rpack)
