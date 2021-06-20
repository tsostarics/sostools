#' Switch reference level in contrast matrix
#'
#' Switch rows to set the reference level properly
#'
#' @param contrast_matrix Contrast matrix, unlabeled
#' @param n_levels Number of levels in the factor
#' @param reference Which row to use as the reference level
#'
#' @return A matrix with the correct values for the reference level
#' @export
.switch_reference_level <- function(contrast_matrix, n_levels, reference){
  if (is.na(reference))
    return(contrast_matrix)
  reference_row <- as.matrix(contrast_matrix[n_levels,]) # as.matrix needed for n_levels = 2
  contrast_matrix[n_levels,] <- as.matrix(contrast_matrix[reference,])
  contrast_matrix[reference,] <- reference_row
  comparison_order <- seq_len(n_levels-1)
  comparison_order <- c(comparison_order[comparison_order != reference], reference)
  as.matrix(contrast_matrix[,comparison_order])
}
