#' CDF for cloglog distribution
#'
#' Works like `plogis`, given a vector of numeric quantiles `x` from `(-Inf,Inf)`,
#' return the cumulative probability up to `x`
#'
#' @param q vector of quantiles
#' @param location location parameter
#' @param scale scale parameter
#'
#' @return cumulative distribution function
#' @export
pcloglog <- function(q,location=0, scale = 1) {
  1 - exp(-exp((x-location)/scale))
}


#' PDF for cloglog distribution
#'
#' Works like `dlogis`, returns the probability density at a given value.
#' Value is equal to the derivative of the CDF defined by
#'
#' @param x vector of quantiles
#' @param location location parameter
#' @param scale scale parameter
#'
#' @return Probability density function
#' @export
dcloglog <- function(x, location = 0, scale = 1) {
  exp((x-location)/scale - exp((x-location)/scale))/scale
}

#' Inverse CDF for cloglog distribution
#'
#' Works like `qlogis`, returns numeric value for cloglog distribution with
#' given location and scale parameters and a probability `p`
#'
#' @param p numeric vector of probabilities `(0,1)`
#' @param location location parameter
#' @param scale scale parameter
#'
#' @return quantile for cloglog distribution of given parameterization
#' @export
qcloglog <- function(p, location = 0, scale = 1) {
  location + scale * log(-log(1-p))
}
