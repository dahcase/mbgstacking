% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_covariates.R
\name{extract_covariates}
\alias{extract_covariates}
\title{Extract Covariate Values}
\usage{
extract_covariates(xyt, covariate_list, centre_scale = T, time_var = "year",
  time_scale = c(2000, 2005, 2010, 2015))
}
\arguments{
\item{xyt}{data.table. A data frame containing columns for latitude, longitude and some time element (defined by time_var)
where raster values should be extracted.}

\item{covariate_list}{list or raster-like. A list (or single raster-like object) of raster-like objects. These normally represent covariates.}

\item{centre_scale}{Logical. Should the covariate values be centered/normalized? Binary variables are excluded.
Binary variables are excluded. Factor variables should be converted into individual rasters using raster::layerize.
This is due to computational friendly-ness and that downstream processes don't always play well with factor variables.}

\item{time_var}{Character. Column in the dataset representing the time variable.}

\item{time_scale}{Numeric Vector. Denotes the full range of times under analysis.
For example, if we are analyzing yearly from 2000-2015, the
the vector should be 2000:2015. if specified, this will be used
to translate between actual time and the suffixes of the covariates.
E.g. 2004 would be translated into 5 (as its the fifth position)}
}
\value{
A data table with columns of the extracted values from covariate list reconciled by time.
        If centre_scale is T, returns the extracted values after centre-scaling (normalizing) as well as
        the centre_scale data frame for later use.
}
\description{
Given a data table with columns for latitude, longitude and some time dimensions, this function extracts values at the intersection of points
(denoted by latitude and longitude) from raster-like objects. From time varying rasters, the function matches the appropriate time using
time_var. extract_covariates accepts either a list of raster-like objects or a raster-like object itself.
Time varying covariates/rasters are infered from the names of the raster like objects by matching the suffix of names(raster_obj)
with the defined time scale. For example, if a raster brick with layer names evi.1, evi.2, evi.3 and evi.4 is passed and the time scale is
2000,2001,2002, and 2003, the function takes evi.2 to represent 2001.
}
