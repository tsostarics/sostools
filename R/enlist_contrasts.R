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
enlist_contrasts <- function(model_data, ...) {
  formulas <- lapply(rlang::enexprs(...), as.character)

  vars_in_model <- vapply(formulas, function(x) x[[2L]] %in% names(model_data), TRUE)
  names(vars_in_model) <- vapply(formulas, function(x) x[[2L]], "char")

  if (!all(vars_in_model))
    stop(glue::glue("{names(vars_in_model)[!vars_in_model]} not found in model data\n",
                    .trim = FALSE))

  stats::setNames(
    lapply(formulas, function(x) .process_contrasts(model_data, x)),
    names(vars_in_model)
  )
}

#' Create contrast code call from formula
#'
#' Helper to generate contrast matrix from passed formulas in enlist_contrasts
#'
#' @param model_data Data frame with factor column
#' @param char_formula character conversion of formula
#'
#' @return A contrast matrix
.process_contrasts <- function(model_data, char_formula) {
  coding_scheme <- strsplit(char_formula[[3L]], " + ", fixed = TRUE)[[1L]]

  generated_call <-
    list(contrast_code,
         factor_col = model_data[[char_formula[[2L]]]],
         code_by = eval(parse(text = coding_scheme[[1L]])),
         reference_level = ifelse(length(coding_scheme) == 2, coding_scheme[[2]], NA)
    )

  eval(as.call(generated_call))
}
