% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_child_raster.R
\name{make_child_raster}
\alias{make_child_raster}
\title{Make rasters from the child model objects}
\usage{
make_child_raster(model_obj, model_settings = NULL, covs, cs_df = NULL,
  indicator_family = "binomial")
}
\arguments{
\item{model_obj}{a model object from stacking}

\item{model_settings}{list. result of a model init giving the instructions on how to fit the model}

\item{covs}{a named list of rasters,}

\item{cs_df}{center scaling df}

\item{indicator_family}{character. Model family}
}
\description{
Using model fits from run_stacking_child_models, creates rasters
}
