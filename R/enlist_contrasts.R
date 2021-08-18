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
#' @param raw_formula Raw formula
#' @param char_formula character conversion of formula
#'
#' @return A contrast matrix
.process_contrasts <- function(model_data, char_formula, raw_formula) {
  var_envir <- rlang::get_env(raw_formula)
  coding_scheme <- strsplit(char_formula[[3L]], " [+*-] ", perl = TRUE)[[1L]][[1L]]

  coding_scheme <- .scrub_scheme(coding_scheme)
  # Including raw matrix calls in the formula requires special handling
  has_matrix <- grepl("matrix\\(",coding_scheme[[1L]])
  if (has_matrix){
    use_matrix <- eval(parse(text = coding_scheme[[1L]]))
    coding_scheme <- "use_matrix"
  }

  params <- .parse_formula(raw_formula, has_matrix)

  # get("columnname", model_data) works the same as model_data$columnname
  contrast_code(
    factor_col = get(params[["factor_col"]], model_data),
    code_by = get(coding_scheme), # May need to
    reference_level = .get_if_exists(params[["reference_level"]], var_envir),
    set_intercept = .get_if_exists(params[["intercept_level"]], var_envir),
    drop_trends = .get_if_exists(params[["drop_trends"]], var_envir)
  )

}

#' Parse contrast formula
#'
#' Takes a formula and figures out the parameters to use based on the operands
#' provided in the formula
#'
#' @param raw_formula Raw formula passed by user
#' @param has_matrix Whether a matrix call is detected or not
#'
#' @return A list of parameters to use for a contrast_code call
.parse_formula <- function(raw_formula, has_matrix = FALSE) {
  no_matrix_string <- char_formula <-  deparse(raw_formula)

  call_parameters <-
    list("factor_col" = NA,
         "reference_level" = NA,
         "intercept_level" = NA,
         "drop_trends" = NA)

  if (has_matrix){
    call_parameters["code_by"] <- "use_matrix"
    no_matrix_string <- gsub(r"(matrix\((.+\(.+\)?)(, .+)*\) ?)","",deparse(raw_formula))
  }

  .check_if_valid_formula(raw_formula, char_formula, no_matrix_string)

  call_parameters[["factor_col"]] <- as.character(raw_formula[[2L]])

  matches <- stringr::str_match_all(no_matrix_string, "([+*-])+ ([^ ]+)")[[1L]]
  if (nrow(matches) == 0L)
    return(call_parameters)

  operations <- matrix(matches[,2:3],nrow = nrow(matches))
  values <- operations[,2L]
  names(values) <- operations[,1L]
  op_mapping <- c("+" = "reference_level",
                  "*" = "intercept_level",
                  "-" = "drop_trends")

  for (operand in names(values)) {
    call_parameters[[op_mapping[[operand]]]] <- values[[operand]]
  }

  call_parameters
}

#' Formula validator
#'
#' Given a formula (and various preprocessed versions of it), run diagnostics
#' to ensure that it's a valid formula.
#'
#' @param formula Raw formula
#' @param char_formula Character string of the formula from deparse
#' @param no_matrix_string String without any matrix calls
#'
#' @return Nothing, throws an error if any are found
.check_if_valid_formula <- function(formula, char_formula, no_matrix_string) {
  if (length(formula) == 1)
    stop("Formula must be two sided.")
  if (grepl("[*+-][^~]+~",no_matrix_string))
    stop("Formula must have 1 variable name on left hand side.")
  if (any(stringr::str_count(no_matrix_string, c("\\+", "\\*", "\\-")) > 1))
    stop("You may only use +, *, and - once")
  if (any(stringr::str_detect(no_matrix_string, c("%in%", "\\^"))))
    stop("You cannot use the ^ or %in% operators in this formula")
  if (grepl("[^-] [^ ]+:[^ ]+", no_matrix_string))
    stop("Sequences of the form a:b may only be used to drop trends with the - operator")
  if (grepl(" ~ [+-].+ ?", char_formula))
    stop("First term in right hand side must be a contrast matrix or contrast function")
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

#' Parse comparisons to drop
#'
#' This parses any sequences following a `-` operator and expands them to
#' a numeric vector.
#'
#' @param scheme_argument A scheme argument of the form "a:b", where a and b
#' can correspond to variable names bound in var_envir or numbers.
#' @param var_envir Environment to look up values if a and b are variable names
#'
#' @return A sequence of numbers
.parse_drop_sequence <- function(scheme_argument, var_envir) {
  sequence_args <- stringr::str_match(scheme_argument,"(.+):(.+)")[,2L:3L]
  # Strings like "3L" cannot be converted to numbers, L must be removed first
  sequence_args <- vapply(sequence_args,
                          function(x)
                            ifelse(grepl("^\\d+L$", x),
                                   gsub("L$","",x),
                                   x),
                          "char")
  numeric_args <- suppressWarnings(vapply(sequence_args, as.numeric, NA_real_))
  arg_is_non_numeric <- is.na(numeric_args)

  sequence_ends <-
    vapply(c(1L,2L),
           function(i) {
             ifelse(arg_is_non_numeric[[i]],
                    get(sequence_args2[[i]], envir=var_envir),
                    numeric_args[[i]])
           },
           1.0)

  seq(sequence_ends[[1L]], sequence_ends[[2L]])
}

