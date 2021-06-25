#' Scaled sum coding
#'
#' For n levels of factors, generate a matrix with n-1 comparisons where:
#' Reference level = -1/n
#' Comparison level = (n-1)/n
#' All others = -1/n
#'
#' @param n_levels number of levels for this factor
#'
#' @return A matrix of scaled sum coded contrasts, unlabeled
#' @export
scaled_sum_code <- function(n_levels) {
  contrast_matrix <- contr.sum(n_levels)
  contrast_matrix[contrast_matrix == 1] <- (n_levels-1) / n_levels
  contrast_matrix[contrast_matrix == 0] <- -1/n_levels
  contrast_matrix[contrast_matrix == -1] <- -1/n_levels

  contrast_matrix
}

