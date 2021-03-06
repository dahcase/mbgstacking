% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{build_parameter_grid}
\alias{build_parameter_grid}
\title{Build Parameter Grid}
\usage{
build_parameter_grid(..., grid_type = "ordered")
}
\arguments{
\item{...}{... Named list of parameters}

\item{grid_type}{character vector. One of ordered, none or all. None returns the first value for each set of parameters,
ordered returns a data frame where each row refers to the position as passed and all returns all combinations.}
}
\description{
Given a set of paramters (e.g. named list). Returns a data frame where each row is a set of parameters to inform
a given model
}
