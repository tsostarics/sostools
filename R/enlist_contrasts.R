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
                                char_formula = char_formulas[[x]],
                                raw_formula = formulas[[x]] # Reference value bindings
             )
    ),
    names(vars_in_model)
  )
}

#' Pass arguments to contrast code
#'
#' Processes a formula and any arguments for the contrast matrix and sets up
#' the contrast_code call
#'
#' @param model_data Data frame with factor column
#' @param raw_formula Raw formula
#' @param char_formula character conversion of formula
#'
#' @return A contrast matrix
.process_contrasts <- function(model_data, char_formula, raw_formula) {
  var_envir <- rlang::get_env(raw_formula)

  params <- .parse_formula(raw_formula)

  if (params[["code_by"]] == "use_matrix"){
    use_matrix <- attr(params[["code_by"]], "mat")
  }

  # get("columnname", model_data) works the same as model_data$columnname
  contrast_code(
    factor_col = get(params[["factor_col"]], model_data),
    code_by = get(params[["code_by"]]),
    reference_level = .get_if_exists(params[["reference_level"]], var_envir),
    set_intercept = .get_if_exists(params[["intercept_level"]], var_envir),
    drop_trends = .get_if_exists(params[["drop_trends"]], var_envir)
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

#' Get value if it exists
#'
#' Given a name and an environment, lookup its value. If it isn't a variable name
#' it will do some string preprocessing before returning the value. Later
#' functions use mostly string representations anyways.
#'
#' @param scheme_argument Argument used in the scheme
#' @param var_envir Environment to look up value
#'
#' @return Value of a variable, or the original string cleaned up
.get_if_exists <- function(scheme_argument, var_envir) {
  if (is.na(scheme_argument))
    return(NA)
  if (exists(scheme_argument,where = var_envir))
    return(get(scheme_argument, envir = var_envir))
  if (grepl("^.+:.+$", scheme_argument))
    return(.parse_drop_sequence(scheme_argument, var_envir))

  gsub('"| ', '', scheme_argument)
}

