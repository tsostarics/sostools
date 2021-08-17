#' List of contrast matrices
#'
#' Returns a list of contrast matrices to use with modeling functions directly.
#'
#' Typically model functions like lm will have a contrasts argument where you can
#' set the contrasts at model run time, rather than having to manually change
#' the contrasts on the underlying factor columns in your data. If you prefer
#' this way, you can use this to generate the named list of contrast matrices.
#'
#'
#' @param model_data Data frame you intend on passing to your model
#' @param ... A series of 2 sided formulas with factor name on the LHS and
#' desired contrast scheme on the RHS, reference levels can be set with + and the
#' intercept can be overwritten with * (+ should come first if both are set)
#' @param verbose Should messages be sent to user? Defaults to TRUE, MUST be
#' a named argument, or it will get parsed as part of ...
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
#' enlist_contrasts(my_df, gear ~ scaled_sum_code)
#'
enlist_contrasts <- function(model_data, ...,  verbose=TRUE) {
  # Get the formulas from the dots into list and character formats to work with
  formulas <- suppressWarnings(rlang::dots_splice(...)) # outer names warning?
  char_formulas <- lapply(formulas, as.character)

  # Extract which factor columns are attempting to be set
  vars_in_model <- vapply(char_formulas, function(x) x[[2L]] %in% names(model_data), TRUE)
  names(vars_in_model) <- vapply(char_formulas, function(x) x[[2L]], "char")

  model_data <- .convert_to_factors(model_data, names(vars_in_model), verbose)

  .msg_if_remaining_factors(model_data, names(vars_in_model))

  if (!all(vars_in_model))
    stop(glue::glue("{names(vars_in_model)[!vars_in_model]} not found in model data\n",
                    .trim = FALSE))

  formula_indices <- seq_along(char_formulas)
  stats::setNames(
    lapply(formula_indices,
           function(x)
             .process_contrasts(model_data,
                                char_formulas[[x]],
                                var_envir = rlang::get_env(formulas[[x]]) # Reference value bindings
                                )
           ),
    names(vars_in_model)
  )
}

#' Convert non factors to factors
#'
#' Helper to convert columns to factors if they aren't already
#'
#' @param model_data Model data
#' @param vars_in_model variables specified for contrast coding from formulas
#' @param verbose Should messages be sent? Defaults to TRUE
.convert_to_factors <- function(model_data, vars_in_model, verbose = TRUE) {
  which_not_factors <- vapply(model_data[vars_in_model],
                              function(x) !is.factor(x),
                              TRUE)

  # Message user if they're resetting an ordered column
  if (verbose) .msg_if_ordered_reset(model_data, vars_in_model)

  # If all specified variables are already factors, no need for coercion
  if (all(!which_not_factors))
    return(model_data)

  # Coerce specified columns to factor data type, send message if needed
  which_to_factors <- names(which_not_factors)[which_not_factors]
  if (verbose) .msg_if_coerced_to_factors(which_to_factors)

  dplyr::mutate(model_data, dplyr::across(dplyr::all_of(which_to_factors), factor))
}


#' Pass arguments to contrast code
#'
#' Helper to generate contrast matrix from passed formulas in enlist_contrasts
#'
#' @param model_data Data frame with factor column
#' @param char_formula character conversion of formula
#' @param var_envir Environment in which the reference variables can be found
#'
#' @return A contrast matrix
.process_contrasts <- function(model_data, char_formula, var_envir) {
  reference_specified <- grepl("\\+",char_formula[[3L]])
  coding_scheme <- strsplit(char_formula[[3L]], " \\+|\\* ", perl = TRUE)[[1L]]
  coding_scheme <- .scrub_scheme(coding_scheme)
  arg_length <- length(coding_scheme)
  reference_level <- NA
  intercept_level <- NA

  # If three arguments are specified, 2nd is ref level & 3rd is intercept level
  # otherwise the second level depends on whether + or * was used
  if (arg_length == 3) {
    reference_level <- .get_if_exists(coding_scheme[[2L]], var_envir)
    intercept_level <- .get_if_exists(coding_scheme[[3L]], var_envir)
  } else if (arg_length == 2) {
    if (reference_specified)
      reference_level <- .get_if_exists(coding_scheme[[2L]], var_envir)
    else
      intercept_level <- .get_if_exists(coding_scheme[[2L]], var_envir)
  }

  # Additional handling if raw matrix is passed
  if (grepl("matrix\\(",coding_scheme[[1L]])) {
    use_matrix <- eval(parse(text = coding_scheme[[1L]]))
    coding_scheme[[1L]] <- 'use_matrix'
  }

  contrast_code(
    factor_col = model_data[[char_formula[[2L]]]],
    code_by = get(coding_scheme[[1L]]),
    reference_level = reference_level,
    set_intercept = intercept_level
  )

}

#' Remove whitespace
#'
#' small helper to trim whitespace
#'
#' @param coding_scheme coding scheme in enlist_contrasts
#'
#' @return new coding scheme char vector trimmed whitespace
.scrub_scheme <- function(coding_scheme) {
  vapply(coding_scheme, function(x) gsub(" *", "", x), "char", USE.NAMES = FALSE)
}

.get_if_exists <- function(scheme_argument, var_envir) {
  if (exists(scheme_argument,where = var_envir))
    return(get(scheme_argument, envir = var_envir))
  gsub('"| ', '', scheme_argument)
}

