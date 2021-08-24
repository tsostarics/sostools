#' Decompose contrasts into new columns
#'
#' First draft of pulling out the contrast coding numbers from the model matrix
#' and adding them into new columns. Currently only handles 1 column at a time
#' and does not work within `mutate`, which is where it would be most helpful.
#'
#' @param model_data Data frame given to model
#' @param factor_col Which column (as a string)
#' @param extract.seq integer vector of which components to get, use NULL (default)
#' to extract all of them
#' @param extract.to Optional names to give the new columns, must equal number of
#' extracted components.
#'
#' @return model_data but with new columns corresponding to the numeric coding
#' of the given factor's contrasts
#'
#' @importFrom stats model.matrix
#' @export
decompose_contrasts <- function(model_data, factor_col, extract.seq = NULL, extract.to = NULL) {
  # insert tidy eval for factor col here for unquoted column later
  # insert vectorized support for multiple columns later

  if (missing(extract.seq))
    extract.seq <- seq_len(nlevels(model_data[[factor_col]]))[-1]

  components <- model.matrix(formula(paste("~",factor_col)), model_data)[,extract.seq+1]
  if (!missing(extract.to)){
    if (length(extract.to) != length(extract.seq))
      stop("Number of names in extract.to should equal number of desired components")
    colnames(components) <- extract.to
  }
  cbind(model_data, components)
}
