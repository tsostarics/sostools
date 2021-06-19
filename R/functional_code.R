#' Set contrast coding with function
#'
#' @param factor_col Factor column
#' @param coding_fx A function that generates contrast matrices
#' @param reference_level Label of the desired reference level, will default to
#' the first label in dimnames of the original contrasts
#'
#' @export
functional_code <- function(factor_col, coding_fx, reference_level) {
  labels <- dimnames(contrasts(factor_col))

  if (missing(reference_level))
    reference_level <- labels[[1L]][1L]
  reference_i <- which(labels[[1]]==reference_level)

  if(identical(reference_i, integer(0)))
    stop("Reference level not found in contrast dimension names")

  n_levels <- length(labels[[1]])

  new_contrasts <- .switch_reference_level(coding_fx(n_levels), n_levels, reference_i)

  dimnames(new_contrasts) <- labels
  .reset_comparison_labels(new_contrasts)
}


