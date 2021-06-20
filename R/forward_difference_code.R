#' Forward difference code
#'
#' Compares the mean of level k to level k+1. Differs from helmert coding because
#' it doesn't compare k to mean(k_i, ..., k_n)
#' Intercept is the grand mean (mean of cell means)
#'
#' @param n_levels Number of factor levels
#'
#' @return Forward difference contrast matrix
#' @export
forward_difference_code <- function(n_levels) {
  contrasts <-
    lapply(seq_len(n_levels),
           function(i)
             c(rep(n_levels-i, i), rep(-i, n_levels-i)) / rep(n_levels, n_levels))
  matrix(unlist(contrasts[seq_len(n_levels-1L)]),
         nrow = n_levels)
}

#' Backward difference code
#'
#' Compares the mean of level k to level k-1. Differs from reverse helmert coding
#' because it doesn't compare k to mean(k_0, ..., k_i-1). Mathematically just
#' flipping the signs of the matrix from forward difference coding.
#'
#' Intercept is the grand mean (mean of cell means)
#'
#' @param n_levels Number of factor levels
#'
#' @return Backward difference contrast matrix
#' @export
backward_difference_code <- function(n_levels) {
  -forward_difference_code(n_levels)
}
