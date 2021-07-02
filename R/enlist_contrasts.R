#' List of contrast matrices
#'
#' Typically model functions like lm will have a contrasts argument where you can
#' set the contrasts at model run time, rather than having to manually change
#' the contrasts on the underlying factor columns in your data. If you prefer
#' this way, you can use this to generate the named list of contrast matrices.
#'
#' @param model_data Data frame you intend on passing to your model
#' @param ... A series of 2 sided formulas with factor name on the LHS and
#' desired contrast scheme on the RHS, reference levels can be set with +
#'
#' @return List of named contrast matrices
#' @export
#'
#' @examples
#' my_df <- mtcars
#' my_df$gear = factor(my_df$gear)
#' my_df$carb = factor(my_df$carb)
#'
#' # Use formulas where left hand side is the factor column name
#' # and the right hand side is the contrast scheme you want to use
#' enlist_contrasts(my_df,
#'     gear ~ scaled_sum_code, # Using helpers from this package
#'     carb ~ helmert_code)
#'
#' # Add reference levels with +
#' enlist_contrasts(my_df,
#'     gear ~ scaled_sum_code + 5,
#'     carb ~ contr.sum + 6)
#'
#' # Manually specifying matrix also works
#' enlist_contrasts(my_df,
#'     gear ~ matrix(c(1,-1,0,0,-1,1), nrow = 3),
#'     carb ~ forward_difference_code)
#'
#' # User matrices can be assigned to a variable first, but this may make the
#' # comparison labels confusing. You should rename them manually to something
#' # that makes sense. This will invoke manual_code, so reference levels
#' # specified with + will be ignored.
#' my_gear_contrasts <- matrix(c(1,-1,0,0,-1,1), nrow = 3)
#' colnames(my_gear_contrasts) <- c("CMP1", "CMP2")
#' enlist_contrasts(my_df,
#'     gear ~ my_gear_contrasts,
#'     carb ~ forward_difference_code)
#'
#'
#' # Will inform you if there are factors you didn't set
#' enlist_contrasts(my_df,
#'     gear ~ scaled_sum_code)
enlist_contrasts <- function(model_data, ...) {
  formulas <- lapply(rlang::enexprs(...), as.character)

  vars_in_model <- vapply(formulas, function(x) x[[2L]] %in% names(model_data), TRUE)
  names(vars_in_model) <- vapply(formulas, function(x) x[[2L]], "char")

  .check_remaining_factors(model_data, names(vars_in_model))

  if (!all(vars_in_model))
    stop(glue::glue("{names(vars_in_model)[!vars_in_model]} not found in model data\n",
                    .trim = FALSE))

  stats::setNames(
    lapply(formulas, function(x) .process_contrasts(model_data, x)),
    names(vars_in_model)
  )
}

#' Pass arguments to contrast code
#'
#' Helper to generate contrast matrix from passed formulas in enlist_contrasts
#'
#' @param model_data Data frame with factor column
#' @param char_formula character conversion of formula
#'
#' @return A contrast matrix
.process_contrasts <- function(model_data, char_formula) {
  coding_scheme <- strsplit(char_formula[[3L]], " + ", fixed = TRUE)[[1L]]
  reference_level <- NA
  if (length(coding_scheme) == 2)
    if (exists(coding_scheme[[2L]]))
      reference_level <- get(coding_scheme[[2L]])
    else
      reference_level <- gsub('"', '', coding_scheme[[2L]])

  contrast_code(
    factor_col = model_data[[char_formula[[2L]]]],
    code_by = get(coding_scheme[[1L]]),
    reference_level = reference_level
  )

}

.check_remaining_factors <- function(model_data, specified_vars) {
  column_classes <- lapply(model_data, class)
  factor_cols <- column_classes == "factor"
  factor_cols <- factor_cols[factor_cols]
  col_names <- names(factor_cols)[factor_cols]

  remaining_factors <- factor_cols[!col_names %in% specified_vars]
  if (any(remaining_factors))
    message(paste0("You didn't set these factors, expect dummy coding: ",
                   paste(names(remaining_factors), collapse = ", ")
                   )
            )

}
