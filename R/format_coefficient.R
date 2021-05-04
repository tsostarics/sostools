#' Format coefficients
#'
#' For easily writing coefficient values in text write up. Note this currently
#' ONLY works for models with zvalues (lm, glm, lmer, glmer). Can be generalized
#' later but I don't want to right now.
#'
#' @param coeff Which coefficient to format
#' @param clist The list of coefficient data to use
#'
#' @return A string of coefficient descriptions
#' @export
format_coef <- function(coeff, clist) {
  p.value <- format_pval(clist[[coeff]][[ncol(clist[[coeff]])]])
  beta <- clist[[coeff]][["Estimate"]]
  se <- clist[[coeff]][["StdError"]]
  stat_name <- names(clist[[coeff]][3L])
  stat_var <- gsub("value", "", stat_name)
  stat_val <- clist[[coeff]][[stat_name]]

  glue::glue("$(\\beta = {beta}, {stat_var} = {stat_val}, s.e. = {se}, {p.value})$")
}
