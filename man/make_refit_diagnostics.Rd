% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brms_fit_check.R
\name{make_refit_diagnostics}
\alias{make_refit_diagnostics}
\title{Create a refit diagnostic list}
\usage{
make_refit_diagnostics(value)
}
\arguments{
\item{value}{logical, either TRUE or FALSE}
}
\value{
named list of logical values.
}
\description{
Helper to create a list of diagnostics filled with the given logical
value. Mainly used with FALSE, but also used with TRUE to return early when
no saved fit is found.
}
