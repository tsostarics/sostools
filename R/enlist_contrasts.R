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
enlist_contrasts <- function(model_data, ...) {
  formulas <- suppressWarnings(rlang::dots_splice(...)) # outer names warning?
  char_formulas <- lapply(formulas, as.character)
  formula_indices <- seq_along(char_formulas)
  vars_in_model <- vapply(char_formulas, function(x) x[[2L]] %in% names(model_data), TRUE)
  names(vars_in_model) <- vapply(char_formulas, function(x) x[[2L]], "char")

  model_data <- .convert_to_factors(model_data, names(vars_in_model))

  .check_remaining_factors(model_data, names(vars_in_model))

  if (!all(vars_in_model))
    stop(glue::glue("{names(vars_in_model)[!vars_in_model]} not found in model data\n",
                    .trim = FALSE))

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
#' @param verbose Should message be sent? Defaults to TRUE
.convert_to_factors <- function(model_data, vars_in_model, verbose = TRUE) {
  which_not_factors <- vapply(model_data[vars_in_model],
                              function(x) !is.factor(x),
                              TRUE)

  # Send message if the factor is ordered (i dont think it's an issue but idk?)
  which_are_ordered <- vapply(model_data[vars_in_model], is.ordered, TRUE)
  any_ordered <- any(which_are_ordered)
  if (verbose & any_ordered) {
    ordered_names <- crayon::blue(paste(names(which_are_ordered)[which_are_ordered], collapse = ' '))
    message(glue::glue("These factors are also ordered: {ordered_names}"))
  }

  # If all specified variables are already factors, we're good
  if (all(!which_not_factors))
    return(model_data)

  should_be_factors <- names(which_not_factors)[which_not_factors]
  varnames <- crayon::blue(paste(should_be_factors, collapse = ' '))

  if (verbose)
    message(glue::glue("Converting these to factors: {varnames}"))

  dplyr::mutate(model_data, dplyr::across(dplyr::all_of(should_be_factors), factor))
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

#' Check for unspecified factors
#'
#' Sends a message if the user has factor columns in their model data frame
#' that weren't specified with the others.
#'
#' @param model_data Model data
#' @param specified_vars variables specified by the user from formulas
#'
#' @return nothing, just sends a message if needed
.check_remaining_factors <- function(model_data, specified_vars) {
  column_classes <- lapply(model_data, class)
  factor_cols <- column_classes == "factor"
  factor_cols <- factor_cols[factor_cols]
  col_names <- names(factor_cols)[factor_cols]

  remaining_factors <- factor_cols[!col_names %in% specified_vars]
  if (any(remaining_factors)) {
    varnames <- crayon::blue(paste(names(remaining_factors), collapse = " "))
    message(glue::glue("You didn't set these factors, expect dummy coding: {varnames}"))
  }
}
