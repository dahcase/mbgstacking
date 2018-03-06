#' Simulate data for unit testing
#'
#' Simulate data to test stacking functions
#'
#' @param seed numeric. to be a random seed
#' @param n_ntc numeric. number of non time varying covariates
#' @param n_tv numeric. number of time varying covariates to include
#' @param n_cells numeric. number of cells in the underlying rasters. sqrt(n_cells) must return an integer
#' @param n_datapoints numeric. number of datapoints to return
#' @param years numeric. vector of the years to be represented
#' @param year_weights numeric. Vector of weights (with length(year_weights) == to length(years)) for stratified sampling of datapoints.
#'                              NULL implies no year based stratification, while the value implies the number of data points (e.g. weight * n_datapoints) for a given year.
#' @param family character. Statistical family of the output. Currently only biomial and gaussian are supported
#' @param fraction_weighted numeric. Specifies the fraction of the dataset should have a weight 0<x<1
#' @return a data set and corrosponding rasters
#' @import data.table
#' @export
#'
simulate_stacking_data = function(seed = 1, n_ntv = 2, n_tv = 5, n_cells = 10000,
                                  n_datapoints = length(years) * 100, years = 2000:2016,
                                  year_weights =rep.int(1/length(years), length(years)), family = 'gaussian',
                                  fraction_weighted = .2){
  #check inputs
  stopifnot((sqrt(n_cells)) %% 1 ==0)
  stopifnot(length(year_weights) == length(years))
  stopifnot(sum(year_weights) == 1)
  set.seed(seed)

  #make template
  template = raster::raster(matrix(rep.int(1, n_cells), ncol = sqrt(n_cells)))

  #make the starting points for covariates
  covs = lapply(1:(n_ntv+ n_tv), function(x) make_random_cov(template = template,
                                                             nu       = 1,
                                                             sd      = abs(rnorm(1,0,.5)),
                                                             scale    = runif(n = 1,min = .2,max = 3)))
  #make some time varying covariates
  covs[1:n_tv] = lapply(1:n_tv, function(x) make_random_tvcov(covs[[x]], nperiods = length(years)))
  names(covs) = paste0(c(rep('n_tv_', times = n_tv), rep('n_ntv_', times = n_ntv)), letters[1:(n_ntv+n_tv)])

  #make a fake outcome variable
  betas = rnorm(n_ntv + n_tv,0 ,.2)
  add <- function(x) Reduce("+", x)
  outcome = add(lapply(1:length(betas), function(x) covs[[x]] * betas[[x]]))

  #pretend outcome is the logit if binomial
  if(family == 'binomial'){
    outcome = invlogit(outcome)
  }

  id = outcome
  id[] = 1:length(outcome)

  #simulate some data points
  if(is.null(year_weights)){
    data_locs = sample(x = id[],size = n_datapoints,replace = T)
  } else{
    data_locs =unlist(lapply(1:dim(outcome)[3], function(x) sample(as.vector(id[[x]]),
                                                                   size = n_datapoints * year_weights[[x]])))
  }

  #get the xyt of those points
  dat = as.data.frame(id, xy = T)
  setDT(dat)
  dat = melt(dat,id.vars = c('x','y'), variable.factor = F, value.factor = F)
  dat[, year:= as.numeric(substr(variable, 7,99)) + 1999]
  dat = dat[value %in% data_locs,]

  #randomly make an indicator
  res = as.vector(outcome)[dat$value]
  dat[, outcome := res ]

  #make an indicator column
  dat[, the_ind := outcome] #keep it the same if gaussian

  #make an N column just for kicks
  dat[, N := round(runif(nrow(dat), 10,1000))]
  if(family == 'binomial'){
    dat[, the_ind := round(the_ind * N)]
  }

  #randomly make some weights
  dat[, weight := 1]
  dat[sample(1:nrow(dat),size = nrow(dat) * fraction_weighted,replace = F), weight := runif(nrow(dat) * fraction_weighted, .01,.9)]

  #subset data columns
  dat = dat[,.(x,y,year,outcome,the_ind,N,weight)]

  return(list(dat, covs, outcome))
}

#' Make random covariate
#'   Makes a raster from a random field
#'
make_random_cov = function(template, nu, sd, scale){
  mod = RandomFields::RMmatern(nu    = nu,
                               var   = sd,
                               scale = scale)
  sims = RandomFields::RFsimulate(model=mod,
             x=sp::coordinates(template)[,1],
             y=sp::coordinates(template)[,2])@data[,1]

  ras = template

  #add a touch of noise

  ras[] = sims + sample(sims, length(sims)) * .5
  return(ras)
}

#' Make timevarying covariate
#'
#'
#' @param start_point raster. A raster that is the starting point for a projection into nperiods
#' @param nperiods numeric. The number of layers the resulting brick should have. Alternatively, the number of
#'                          years the covariate should represent.
#' @return A rasterbrick with `nperiods` layers projected forward from some starting point
make_random_tvcov = function(start_point, nperiods){

  #make a beta coefficient
  beta = rnorm(1, 0,.1)

  ras = lapply(1:nperiods, function(x){
    start_point + #starting point
    ((x -1) *(     #will leave the start point unchanged if its the first year
    beta  *
    (x*.05) * start_point)) #reduce temporal trend
  })

  return(raster::brick(ras))


}
