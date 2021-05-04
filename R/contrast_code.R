#' Contrast Code with Labels
#'
#' This makes it easier to add contrast coding schemes while retaining the
#' labels of the levels IN THE ORDER THEY APPEAR IN THE ORIGINAL CONTRAST
#' SCHEME. Usually in alphabetical order. Will default to scaled sum coding
#' for 2 level factors with first alphabetically as the reference level. If
#' it's greater than 2 levels you MUST provide your own matrix using the matrix
#' function.
#'
#' @param factor_col The factor column to use, eg data$gender
#' @param coding_matrix The matrix you want to use, obligatory if n factors > 2
#'
#' @return A matrix for contrast coding schemes
#' @export
#'
#' @importFrom stats contr.sum
contrast_code <- function(factor_col, coding_matrix = NA) {
  if (any(is.na(coding_matrix)) & length(levels(factor_col)) > 2) {
    stop("This factor has more than 2 levels, please provide a matrix.")
  }

  labels <- dimnames(contrasts(factor_col))
  if (!any(is.na(coding_matrix))) {
    new_contrasts <- coding_matrix
  } else {
    new_contrasts <- -contr.sum(2) / 2
  }

  dimnames(new_contrasts) <- labels
  new_contrasts
}
