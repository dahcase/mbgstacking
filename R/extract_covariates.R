#' Extract Covariate Values
#'
#' Given a data table with columns for latitude, longitude and some time dimensions, this function extracts values at the intersection of points
#' (denoted by latitude and longitude) from raster-like objects. From time varying rasters, the function matches the appropriate time using
#' time_var. extract_covariates accepts either a list of raster-like objects or a raster-like object itself.
#' Time varying covariates/rasters are infered from the names of the raster like objects by matching the suffix of names(raster_obj)
#' with the defined time scale. For example, if a raster brick with layer names evi.1, evi.2, evi.3 and evi.4 is passed and the time scale is
#' 2000,2001,2002, and 2003, the function takes evi.2 to represent 2001.
#'
#'
#' @param xyt data.table. A data frame containing columns for latitude, longitude and some time element (defined by time_var)
#'                        where raster values should be extracted.
#' @param covariate_list list or raster-like. A list (or single raster-like object) of raster-like objects. These normally represent covariates.
#' @param centre_scale Logical. Should the covariate values be centered/normalized? Binary variables are excluded.
#'                            Binary variables are excluded. Factor variables should be converted into individual rasters using raster::layerize.
#'                            This is due to computational friendly-ness and that downstream processes don't always play well with factor variables.
#' @param time_var Character. Column in the dataset representing the time variable.
#' @param time_scale Numeric Vector. Denotes the full range of times under analysis.
#'                  For example, if we are analyzing yearly from 2000-2015, the
#'                  the vector should be 2000:2015. if specified, this will be used
#'                  to translate between actual time and the suffixes of the covariates.
#'                  E.g. 2004 would be translated into 5 (as its the fifth position)
#' @return A data table with columns of the extracted values from covariate list reconciled by time.
#'         If centre_scale is T, returns the extracted values after centre-scaling (normalizing) as well as
#'         the centre_scale data frame for later use.
#' @import data.table
#' @export
#'
extract_covariates = function(xyt, covariate_list, centre_scale = T, time_var = 'year', time_scale = c(2000,2005,2010,2015)){

  #deal with data table scoping
  xyt = copy(xyt)

  #check for names
  check_names(xyt, c('latitude','longitude',time_var))

  #if a list, condense to a brick-- outcome should be the same
  if(class(covariate_list) == 'list'){
    covariate_list = raster::brick(covariate_list)
  }

  #subset to the three required columns
  xyt = xyt[, c('latitude','longitude',time_var), with = F]

  #create a row id
  xyt = xyt[,('rid') := 1:nrow(xyt)]

  #create a variable

  #extract the covariates
  cov_values = raster::extract(covariate_list, xyt[, c('longitude', 'latitude'), with = F])

  #check to make sure

  #combine them
  xyt = cbind(xyt, cov_values)

  #create a column to interface with the covariate suffixes (namely period format)
  if(!is.null(time_scale)){
    xyt[,('time_id') := match(get(time_var), time_scale)]
  } else{
    xyt[,('time_id') := get(time_var)]
  }

  #make sure time id is not null
  if(nrow(xyt[is.na(get('time_id')),])>0){
    stop('Not all rows have a valid time id. Please check time scale')
  }

  #extract the column names of time varying covariates and their corrosponding stems

  tv_cov_col_names = grep("(\\.[0-9]+)$",names(xyt), value =T)
  idv = names(xyt)[!names(xyt) %in% tv_cov_col_names]

  #remove trailing periods followed by numbers at the end of the string
  tv_cov_col_names_uniq = unique(gsub('\\.[0-9]*$', '', tv_cov_col_names))

  #organize into a list
  tv_cov_col_names = lapply(tv_cov_col_names_uniq, function(x) grep(x, tv_cov_col_names, value = T))

  #melt
  xyt = data.table::melt(xyt, id.vars = idv,
            measure = tv_cov_col_names, value.name = tv_cov_col_names_uniq, variable.factor =F)

  #If only one time varying covariate, make the names behave
  if(length(tv_cov_col_names_uniq)==1){
    xyt = xyt[,('variable'):= sub(paste0(tv_cov_col_names_uniq,'.'), "", variable, fixed = T)]
  }

  #R has something funny where 1 == '1'. Although this is helpful in our case, I'm casting as an int, because such actions are weird
  xyt = xyt[, as.integer(get('variable'))]

  #keep only rows where there is data time/covariate time agreement
  xyt = xyt[get('time_id') == get('variable') ,]

  #subset so that only the covariate columns are kept
  data.table::setorderv(xyt, 'rid')

  #create names of covariates
  covnames = unique(gsub('\\.[0-9]*$', '', names(covariate_list)))

  xyt = xyt[,covnames, with =F]

  if(centre_scale){
    cs_df = getCentreScale(xyt, exclude = find_binary(xyt))
    xyt = data.table::as.data.table(centreScale(xyt, cs_df))
    return(list(xyt, cs_df))
  } else{
    return(list(xyt))
  }
} #close extract covariates

