library(devtools)
rpack = '/share/geospatial/stacking_packages2/'
for(ppp in c('data.table', 'raster', 'earth', 'mgcv', 'glmnet', 'parallel')){
  install.packages(ppp, lib = rpack)
}

#R 3.3.2
#Not working at the moment; xgboost won't install, even with the new toolset
#system('scl enable devtoolset-4 bash')
rpack = '/share/geospatial/stacking_packages/'
dir.create(rpack)
.libPaths('/share/geospatial/stacking_packages/')
install.packages('devtools', lib = rpack)
install.packages('xgboost', lib = rpack)
devtools::install_github('dahcase/mbgstacking', lib = rpack)

#rnew (3.4)
#scl enable devtoolset-4 bash
rpack = '/share/geospatial/stacking_packages_r34/'
.libPaths('/share/geospatial/stacking_packages_r34/')
devtools::install_github('dahcase/mbgstacking', lib = rpack)

#r conda
#with source activate on
rpack = '/share/geospatial/stack_pack/'
.libPaths('/share/geospatial/stack_pack/')
devtools::install_github('dahcase/mbgstacking', lib = rpack)

#install mbg packages
pos_packs = list.dirs('/home/j/temp/geospatial/packages',recursive = F, full.names = F)
rpack = '/share/geospatial/stack_pack/'
.libPaths('/share/geospatial/stack_pack/')

for(ppp in pos_packs){
  install.packages(ppp, lib = rpack)
}
