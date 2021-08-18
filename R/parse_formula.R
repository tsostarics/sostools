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
  no_matrix_string <- char_formula <-  deparse1(raw_formula)

  call_parameters <-
    list("factor_col" = NA,
         "reference_level" = NA,
         "intercept_level" = NA,
         "drop_trends" = NA)

  if (has_matrix) {
    no_matrix_string <- gsub(r"(matrix\((.+\(.+\)?)(, .+)*\) ?)","",deparse1(raw_formula))
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
  if (length(formula) != 3L)
    stop("Formula must be two sided.")
  if (grepl("[*+-][^~]+~",no_matrix_string))
    stop("Formula must have 1 variable name on left hand side.")
  if (any(stringr::str_count(no_matrix_string, c("\\+", "\\*", "\\-")) > 1))
    stop("You may only use +, *, and - once")
  if (any(stringr::str_detect(no_matrix_string, c("%in%", "\\^"))))
    stop("You cannot use the ^ or %in% operators in this formula")
  if (grepl("[^-] [^ ]+:[^ ]+", no_matrix_string))
    stop("Sequences of the form a:b may only be used to drop trends with the - operator")
  if (grepl(" ~ ([+-]|\\d)", char_formula))
    stop("First term in right hand side must be a contrast matrix or contrast function")
  return(invisible(TRUE))
}
