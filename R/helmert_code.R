#' Reverse helmert code
#'
#' R's contr.helmert function actually implements reverse helmert coding, this
#' extension scales the matrix such that a 1 unit increase corresponds to a
#' change in factor level.
#'
#' Reverse helmert coding compares each level to the total mean of all levels
#' that have come before it. Differs from backward difference coding, which
#' compares only pairs of levels (not a level to a cumulative mean of levels)
#'
#' @param n_levels Number of levels in the factor
#'
#' @return Contrast matrix
#' @export
reverse_helmert_code <- function(n_levels){
  apply(unname(contr.helmert(n_levels)), 2L, function(x) x / sum(x != 0))
}

#' Helmert code
#'
#' Since R's contr.helmert implements reverse helmert coding, we can get
#' regular helmert coding with reversing the reverse helmert matrix.
#'
#' Helmert coding compares each level to the total mean of all levels that come
#' after it. Differs from forward difference coding, which only compares pairs
#' of levels (not a level to a cumulative mean of levels).
#'
#' @param n_levels Number of levels in the factor
#'
#' @return Contrast matrix
#' @export
helmert_code <- function(n_levels){
  matrix(rev(reverse_helmert_code(n_levels)), nrow = n_levels)
}
