#' Set contrast coding with function
#'
#' @param factor_col Factor column
#' @param coding_fx A function that generates contrast matrices
#' @param reference_level Label of the desired reference level, will default to
#' the first label in dimnames of the original contrasts
#'
#' @export
#' @importFrom stats contr.helmert contr.poly contr.treatment
functional_code <- function(factor_col, coding_fx, reference_level=NA) {
  labels <- dimnames(contrasts(factor_col))
  reference_i <- NA
  if (is.na(reference_level))
    reference_i <- which(labels[[1L]]==labels[[1L]][1L])
  else
    reference_i <- which(labels[[1L]] == reference_level)
  if (.check_non_references(coding_fx)) {
    if (!is.na(reference_level))
      warning("Ignoring reference level passed for contrast scheme lacking a singular reference.")
    reference_i <- NA
  }

  if(identical(reference_i, integer(0)))
    stop("Reference level not found in contrast dimension names")

  n_levels <- length(labels[[1L]])

  new_contrasts <- .switch_reference_level(coding_fx(n_levels),
                                           coding_fx,
                                           n_levels,
                                           reference_i)

  dimnames(new_contrasts) <- labels
  .reset_comparison_labels(new_contrasts)
}

.check_non_references <- function(coding_fx) {
  any(
    vapply(c(backward_difference_code,
           forward_difference_code,
           helmert_code,
           reverse_helmert_code,
           contr.poly),
         function(x)
           identical(coding_fx, x),
         TRUE)
  )
}
