% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_propodds.R
\name{test_propodds}
\alias{test_propodds}
\title{Proportional odds Testing}
\usage{
test_propodds(
  ord_data,
  model_formula,
  clm_obj = NULL,
  .link = "logit",
  control = list()
)
}
\arguments{
\item{ord_data}{Data frame with your data}

\item{model_formula}{The model formula to provide to a glm call}

\item{clm_obj}{Optional CLM or CLMM object to compare estimates to}

\item{.link}{Link function, defaults to logit for proportional odds model,
message is sent if this differs from clm_obj's link function}

\item{control}{Control to use for mixed models, defaults to}
}
\value{
A data frame with estimates and confidence intervals for each scale response
}
\description{
Tests for proportional odds assumption in data. You can use as many predictors
as you want, but you cannot use random effects (only fixed effects allowed)
for the time being. A message will be displayed whether if the proportional odds
assumption is likely met or not. A dataframe with estimates and confidence intervals
will be returned if you want to plot th rsults to double check.
}
