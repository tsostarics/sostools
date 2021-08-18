#' Set contrasts to dataframe
#'
#' Uses the same syntax as enlist_contrasts, but returns the dataframe with the
#' new contrasts applied. Use this when your model function doesnt have a contrasts
#' argument and you want to avoid writing contrasts<- multiple times.
#'
#' NOTE: Sometimes when using orthogonal polynomial contrasts from contr.poly,
#' people will drop higher level polynomials for parsimony. Note however that
#' these do capture some amount of variation, so even though they're orthogonal
#' contrasts the lower level polynomials will have their estimates changed. Moreover,
#' you cannot reduce a contrast matrix to a matrix smaller than size k*k-1 in
#' the dataframe you pass to a model fitting function itself, as R will try
#' to fill in the gaps with something else. I don't know what these correpond to,
#' but it's unlikely to be anything meaningful. If you want to drop contrasts
#' you'll need to use something like `enlist_contrasts(df, x ~ contr.poly - 3:5)`
#' and pass this to the `contrasts` argument in the model fitting function.
#'
#' @param model_data Data to be passed to the model, ensure factors are available
#' @param ... Series of formulas denoting which contrast scheme to use for each factor
#'
#' @return the model_data dataframe, but with updated contrasts.
#' @export
set_contrasts <- function(model_data, ...) {
  formulas <- rlang::dots_splice(...)
  formulas <- .reinstate_dropped_trends(formulas)
  contrast_list <- enlist_contrasts(model_data, formulas)
  factor_vars <- names(contrast_list)
  model_data <- .convert_to_factors(model_data, factor_vars, verbose = FALSE)

  for (i in seq_along(contrast_list)) {
    factor_name <- names(contrast_list[i])
    contrasts(model_data[[factor_name]]) <- contrast_list[[i]]
  }

  model_data
}

#' Ignore attempt to drop trends in set_contrasts
#'
#' Will send a warning if it detects dropped trends with orthogonal polynomial
#' contrasts. If you use a different coding scheme it just gets ignored anyways.
#'
#' @param formulas formulas passed to `set_contrasts`
#'
#' @return Cleaned up formulas as needed
.reinstate_dropped_trends <- function(formulas) {
  char_formulas <- vapply(formulas, deparse, "char")
  has_dropped_trends <- vapply(char_formulas, function(x) grepl(" - [^ ]+:[^ ]+", x) & grepl("contr\\.poly", x), TRUE)
  num_ignoring <- sum(has_dropped_trends)

  if (num_ignoring == 0)
    return(formulas)

  ignore_string <- paste(crayon::cyan(num_ignoring), ifelse(num_ignoring == 1, "formula", "formulas"))

  if (any(has_dropped_trends)) {
    warning(glue::glue("Cannot drop trends with set_contrasts, ignoring in {ignore_string}. Use enlist_contrasts instead."))
  }

  formulas_dropped_indices <- which(has_dropped_trends)

  for (i in formulas_dropped_indices) {
    var_envir <- rlang::get_env(formulas[[i]])
    new_formula <- gsub(" - [^ ]+:[^ ]+","",deparse(formulas[[i]]))
    formulas[[i]] <- formula(new_formula, env = var_envir)
  }

  formulas
}
