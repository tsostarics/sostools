#' formula to character error handling
#'
#' Adds a more helpful hint when handling the initial coercion in `enlist_contrasts`
#'
#' @param formulas Series of formulas passed by `enlist_contrasts`
#'
#' @return list of formulas coerced to character, or an error
.formula_to_char <- function(formulas) {
  tryCatch(lapply(formulas, as.character),
           error = function(c) {
             err <- conditionMessage(c)
             if (!grepl("cannot coerce type 'closure' to vector of type 'character'", err))
               stop(c)
             stop(paste(err, "Did you use = instead of ~ when setting the contrast?",sep="\n"))
           })
}
