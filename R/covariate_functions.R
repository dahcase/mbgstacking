#Daniel Casey
#Created: 2-24-2017
#Purpose: Functions that prepare the data to be put through the stacking ensemble
#------------------------------------------------------------------------------#
##functions in this file
#extract_covariates

#' Extract covariate values (from raster-like objects) to points.
#'
#' @param xyt spatialpointsdataframe. A spdf where raster values should be extracted.
#' @param covariate_list List. A list of raster-like objects holding covariate values.
#' @param centre_scale Logical. Should the covariate values be centered?
#' @param time_var Character. Column in the dataset representing the time. Should be matched with the bricks
#' @param build_time_scale Numeric Vector. Denotes the full range of times under analysis.
#'                  For example, if we are analyzing yearly from 2000-2015, the
#'                  the vector should be 2000:2015. if specified, this will be used
#'                  to translate between actual time and the suffixes of the covariates.
#'                  E.g. 2004 would be translated into 5 (as its the fifth position)
#' @return A spdf with columns of the extracted values from covariate list reconciled by time.
#'         if centre_scale is T, returns the resulting data frame.
extract_covariates = function(xyt, covariate_list, centre_scale = T, time_var = 'year', time_scale = c(2000,2005,2010,2015)){

  #deal with data table scoping
  xyt = copy(xyt)

  #check for names
  check_names(xyt, c('latitude','longitude',time_var))

  #subset to the three required columns
  xyt = xyt[, c('latitude','longitude',time_var), with = F]

  #create a row id
  xyt = xyt[,rid := 1:nrow(xyt)]

  #extract the covariates
  cov_values = do.call(cbind,lapply(covariate_list, function(x) extract(x, xyt[,.(longitude, latitude)])))

  #combine them
  xyt = cbind(xyt, cov_values)

  #create a column to interface with the covariate suffixes (namely period format)
  if(!is.null(time_scale)){
    xyt[,time_id := match(get(time_var), time_scale)]
  } else{
    xyt[,time_id := get(time_var)]
  }

  #extract the column names of time varying covariates and their corrosponding stems

  tv_cov_col_names = grep("(\\.[0-9]+)$",names(xyt), value =T)
  idv = names(xyt)[!names(xyt) %in% tv_cov_col_names]
  tv_cov_col_names_uniq = unique(gsub('.{2}$', '', tv_cov_col_names))

  #organize into a list
  tv_cov_col_names = lapply(tv_cov_col_names_uniq, function(x) grep(x, tv_cov_col_names, value = T))

  #melt
  xyt = melt(xyt, id.vars = idv,
            measure = tv_cov_col_names, value.name = tv_cov_col_names_uniq, variable.factor =F)

  #keep only rows where there is data time/covariate time agreement
  xyt = xyt[time_id == variable ,]

  #subset so that only the covariate columns are kept
  setorder(xyt, rid)
  xyt = data.table(xyt[,names(covariate_list), with =F])


  #center scale the result if requested
  xyt = data.table(xyt[,names(covariate_list), with =F])

  if(centre_scale){
    cs_df = getCentreScale(xyt, exclude = find_binary(xyt))
    xyt = as.data.table(centreScale(xyt, cs_df))
    return(list(xyt, cs_df))
  } else{
    return(list(xyt))
  }
} #close extract covariates

