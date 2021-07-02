#' Set contrasts to dataframe
#'
#' Uses the same syntax as enlist_contrasts, but returns the dataframe with the
#' new contrasts applied. Use this when your model function doesnt have a contrasts
#' argument and you want to avoid writing contrasts<- multiple times.
#'
#' @param model_data Data to be passed to the model, ensure factors are available
#' @param ... Series of formulas denoting which contrast scheme to use for each factor
#'
#' @return the model_data dataframe, but with updated contrasts.
#' @export
set_contrasts <- function(model_data, ...) {
  code_call <- match.call()
  code_call[[1L]] <- enlist_contrasts
  contrast_list <- eval(code_call)

  for (i in seq_along(contrast_list)) {
    factor_name <- names(contrast_list[i])
    contrasts(model_data[[factor_name]]) <- contrast_list[[i]]
  }

  model_data
}
