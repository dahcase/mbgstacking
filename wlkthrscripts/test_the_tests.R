#Test random covariate generation
library(raster)
library(data.table)
library(ggplot2)
template = raster::raster(matrix(rep.int(1, 10000), ncol = sqrt(10000)))
cov = make_random_cov(template = template,
                nu       = 1,
                sd      = abs(rnorm(1,0,.1)),
                scale    = 5)
covbrick = make_random_tvcov(cov, length(2000:2016))
blar = setDT(as.data.frame(covbrick))
blar[, id:= .I]
blar = melt(blar, id.vars = 'id')

g = ggplot(blar[id %in% c(1:5,300:305),], aes(x = variable, y = value, group = id)) + geom_line()
plot(g)
