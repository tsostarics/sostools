% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/targets.R
\name{tar_load_err}
\alias{tar_load_err}
\title{Load target from error message}
\usage{
tar_load_err()
}
\value{
nothing, loads object to current environment
}
\description{
In interactive sessions, if a value associated with a target's object is
not found (thus throwing an error), tar_load_err() will read the last
error message and load the missing object
}
