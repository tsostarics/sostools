% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_brmsfit.R
\name{tidy_brmsfit}
\alias{tidy_brmsfit}
\title{Brmsfit fixed effects as tidy data frame}
\usage{
tidy_brmsfit(model, include_prior = TRUE, fix.intercept = FALSE, ...)
}
\arguments{
\item{model}{brmsfit object}

\item{include_prior}{Should description of priors be included?}

\item{fix.intercept}{whether Intercept should be changed to (Intercept)}

\item{...}{Other options to pass to broom.mixed::tidy}
}
\value{
Tidy dataframe of fixed effects for use with other functions in this package
}
\description{
Tidy helper for brmsfit objects
}
\details{
Extracts fixed effects (class=b) and Intercept into a tidy data frame. You can
include the description of the priors used by setting include_prior to TRUE.
Note that I recommend that you specify all priors manually, if you set the default
for a class it should still work but no guarantees with more complex models.
Random effects currently not included, see broom.mixed::tidy for that
(that function doesn't work with models lacking random effects, go figure)
}
