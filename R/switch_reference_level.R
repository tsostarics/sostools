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
.switch_reference_level <- function(contrast_matrix, coding_fx, old_reference, new_reference){
  if (is.na(new_reference))
    return(contrast_matrix)
  if (identical(coding_fx, contr.treatment))
    old_reference <- 1L
  reference_row <- as.matrix(contrast_matrix[old_reference,]) # as.matrix needed for n_levels = 2
  contrast_matrix[old_reference,] <- as.matrix(contrast_matrix[new_reference,])
  contrast_matrix[new_reference,] <- reference_row
  comparison_order <- seq_len(old_reference-1)
  if (identical(coding_fx, contr.treatment))
    return(contrast_matrix)
  if (old_reference != 2) # Avoid dimension length issues with n=2
    comparison_order <- c(comparison_order[comparison_order != new_reference], new_reference)

  as.matrix(contrast_matrix[,comparison_order])
}
