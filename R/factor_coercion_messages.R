#' Alert user when setting ordered factors
#'
#' Ordered factors use orthogonal polynomials (contr.poly) by default with
#' k number of levels -1 (k-1) of trend comparisons. If you use these functions
#' to set the contrasts to something else, the ordered class (hence ordering
#' to the levels) will remain but the contrasts will be set to something else.
#'
#' @param model_data Data frame to be used with the model
#' @param vars_in_model Variables to check against
#'
#' @return Nothing, sends a message
.msg_if_ordered_reset <- function(model_data, vars_in_model) {
  which_are_ordered <- vapply(model_data[vars_in_model], is.ordered, TRUE)
  any_ordered <- any(which_are_ordered)
  if (any_ordered) {
    or_default <- crayon::red(options("contrasts")[[1L]]['ordered'])
    ordered_names <- crayon::red(paste(names(which_are_ordered)[which_are_ordered], collapse = ' '))
    message(glue::glue("These factors are ordered, you may lose {or_default}: {ordered_names}"))
  }
}


#' Alert user when factor coercion happens
#'
#' Sends a message if columns in the dataset have been coerced to a factor,
#' useful if you accidentally set a column with continuous data to a factor.
#'
#' @param which_to_factors Names of columns that have been coerced to factors
#'
#' @return Nothing, sends a message if needed
.msg_if_coerced_to_factors <- function(which_to_factors) {
  varnames <- crayon::blue(paste(which_to_factors, collapse = ' '))
  message(glue::glue("Converting these to factors: {varnames}"))
}


#' Alert user if there are more factors
#'
#' Sends a message if the user has factor columns in their model data frame
#' that weren't specified along with any factors they DID set contrasts for.
#'
#' @param model_data Model data
#' @param specified_vars variables specified by the user from formulas
#'
#' @return nothing, just sends a message if needed
.msg_if_remaining_factors <- function(model_data, specified_vars) {
  column_classes <- lapply(model_data, class)
  which_are_factors <- vapply(column_classes,
                              function(x) "factor" %in% x,
                              FUN.VALUE = TRUE,
                              USE.NAMES = TRUE)
  which_are_ordered <- vapply(column_classes,
                              function(x) "ordered" %in% x,
                              FUN.VALUE = TRUE,
                              USE.NAMES = TRUE)
  # Filter named logical vector to be only those where TRUE
  factor_cols <- which_are_factors[which_are_factors]
  factor_names <- names(factor_cols)

  remaining_factors <- factor_cols[!factor_names %in% specified_vars]

  if (any(remaining_factors)) {
    # Lookup default contrasts and color code accordingly
    uo_default <- crayon::blue(options("contrasts")[[1L]]['unordered'])
    or_default <- crayon::red(options("contrasts")[[1L]]['ordered'])
    default_contrasts <- paste(c(uo_default, or_default), collapse = " or ")
    varnames <- vapply(names(remaining_factors),
                       function(x)
                         ifelse(which_are_ordered[x],
                                crayon::red(x),
                                crayon::blue(x)),
                       "char") |> paste(collapse = " ")
    message(glue::glue("You didn't set these factors, expect {default_contrasts}: {varnames}"))
  }
}
