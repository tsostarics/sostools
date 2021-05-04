#' Get coefficient values from list of coefficients
#'
#' Use get_coeff(coeff, clist) to retrieve the value of coeff from clist
#'
#' Or use function factor make_coef_getter to make it easier to retrieve values from
#' specific lists when you have multiple lists.
#'
#' @param coef Which coefficient to look up
#' @param clist The list of coefficients to use
#'
#' @return Formatted value string for the coefficient
#' @export
#' @importFrom stats setNames
#' @examples
#' mdl <- lm(mpg ~ cyl * gear, data = mtcars) # Create model
#' coefs <- enlist_coefs(mdl) # Create coefficients
#' get_coef1 <- make_coef_getter(mdl) # Using the model object directly
#' get_coef2 <- make_coef_getter(coefs) # Using the list of coefficients we made
#'
#' # All are equivalent
#' get_coef("cyl:gear", coefs)
#' get_coef1("cyl:gear")
#' get_coef2("cyl:gear")
#'
#'
get_coef <- function(coef, clist) {
  clist[[coef]]
}

.make_coef_getter.clist <- function(clist) {
  function(x)
    get_coef(x, clist)
}

.make_coef_getter.mdl <- function(mdl) {
  coefs <- enlist_coefs(mdl)
  function(x) {
    get_coef(x, coefs)
  }
}

#' Make a coefficient getter
#'
#' Create a new function that you can use to retrieve coefficients within
#' and RMD document.
#'
#' @param obj Either a coefficient list from enlist_coefs or a model object
#'
#' @return An estimate string
#' @export
make_coef_getter <- function(obj){
  if (class(obj) == "list")
    return(.make_coef_getter.clist(obj))
  else
    return(.make_coef_getter.mdl(obj))
}
