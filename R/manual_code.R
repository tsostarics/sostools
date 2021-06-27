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

  # given_levels <- rownames(coding_matrix)
  given_comparisons <- colnames(coding_matrix)
  labels <- dimnames(contrasts(factor_col))

  # if (!is.null(given_levels) & !all(rownames(coding_matrix) %in% labels[[1]]))
  #   warning("Factor levels in matrix not the same as data levels, order may not be as intended")

  # Handling if the given matrix has prespecified comparison labels
  # if (!is.null(given_levels))
  #   labels[[1]] <- given_levels

  if (!is.null(given_comparisons))
    labels[[2]] <- given_comparisons

  # Default behavior for 2 level factors
  if (!any(is.na(coding_matrix))) {
    new_contrasts <- coding_matrix
  } else {
    new_contrasts <- -contr.sum(2) / 2
  }

  dimnames(new_contrasts) <- labels
  if (!is.null(given_comparisons))
    return(new_contrasts)
  .reset_comparison_labels(new_contrasts)
}

.reset_comparison_labels <- function(contr_mat) {
  n_levels <- nrow(contr_mat)
  if (.check_polynomial(contr_mat))
    colnames(contr_mat) <- colnames(contr.poly(n_levels))
  else if (n_levels != 2 & .check_backward_difference(contr_mat))
      colnames(contr_mat) <- paste(rownames(contr_mat)[-1], rownames(contr_mat)[-n_levels], sep = "-")
  else if (n_levels != 2 & .check_forward_difference(contr_mat))
      colnames(contr_mat) <- paste(rownames(contr_mat)[-n_levels], rownames(contr_mat)[-1], sep = "-")
  else {
    new_colnames <- unname(apply(contr_mat, 2, \(x) rownames(contr_mat)[x > 0]))
    if (all(vapply(new_colnames, \(x) length(x) == 1, TRUE)))
      colnames(contr_mat) <- new_colnames
  }
  contr_mat
}

.check_polynomial <- function(contr_mat) {
  check_val <- round(contr.poly(nrow(contr_mat))[1], 3)
  check_val %in% round(contr_mat, 3)
}

.check_forward_difference <- function(contr_mat) {
  all(contr_mat[upper.tri(contr_mat, TRUE)] > 0) & all(contr_mat[lower.tri(contr_mat)] < 0)
}

.check_backward_difference <- function(contr_mat) {
  all(contr_mat[upper.tri(contr_mat, TRUE)] < 0) & all(contr_mat[lower.tri(contr_mat)] > 0)
}
