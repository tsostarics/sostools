#' Manual code factors
#'
#' Use this when you want to manually specify the contrast matrix
#'
#' @param factor_col The factor column to use, eg data$gender
#' @param coding_matrix The matrix you want to use, obligatory if n factors > 2
#'
#' @return A matrix for contrast coding schemes
#' @export
#'
#' @importFrom stats contr.sum contrasts
manual_code <- function(factor_col, coding_matrix = NA) {
  if (any(is.na(coding_matrix)) & length(levels(factor_col)) > 2) {
    stop("This factor has more than 2 levels, please provide a matrix.")
  }

  labels <- dimnames(contrasts(factor_col))
  # Default behavior for 2 level factors
  if (!any(is.na(coding_matrix))) {
    new_contrasts <- coding_matrix
  } else {
    new_contrasts <- -contr.sum(2) / 2
  }

  dimnames(new_contrasts) <- labels
  .reset_comparison_labels(new_contrasts)
}

.reset_comparison_labels <- function(contr_mat) {
  if (.check_polynomial(contr_mat))
    colnames(contr_mat) <- contr_mat |> nrow() |> contr.poly() |> colnames()
  else
    colnames(contr_mat) <-
      unname(apply(contr_mat, 2, \(x) rownames(contr_mat)[x > 0]))

  contr_mat
}

.check_polynomial <- function(contr_mat) {
  check_val <- round(contr.poly(nrow(contr_mat))[1], 3)
  check_val %in% round(contr_mat, 3)
}
