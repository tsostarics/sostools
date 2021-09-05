#' Format coefficients
#'
#' For easily writing coefficient values in text write up. Note this currently
#' ONLY works for models with zvalues (lm, glm, lmer, glmer). Can be generalized
#' later but I don't want to right now.
#'
#' @param coeff Which coefficient to format
#' @param clist The list of coefficient data to use
#' @param stat_name Name of the statistic
#' @return A string of coefficient descriptions
#' @export
format_coef <- function(coeff, clist, stat_name) {
  p.value <- format_pval(clist[[coeff]][[ncol(clist[[coeff]])]])
  beta <- clist[[coeff]][["estimate"]]
  se <- clist[[coeff]][["std.error"]]
  # stat_name <- names(clist[[coeff]][4L])
  stat_var <- gsub(" *value", "", stat_name)
  stat_val <- clist[[coeff]][["statistic"]]
  coef_symbol <- ifelse(grepl("\\d\\|\\d", coeff), paste0("\\hat\\theta_{",coeff,"}"), "\\hat\\beta")
  glue::glue("$({coef_symbol} = {beta}, {stat_var} = {stat_val}, s.e. = {se}, {p.value})$")
}

#' Title
#'
#' @param coeff Which coefficient to format
#' @param clist The list of coefficient data to use
#'
#' @return Formatted coefficient string
#'
.format_brms_coef <- function(coeff, clist){
  beta <- vapply(clist[[coeff]][["estimate"]], function(x) round(x, 2), 1.0)
  se <- clist[[coeff]][["est.error"]]
  conf_int <- clist[[coeff]][["confint"]]
  coef_symbol <- ifelse(grepl("\\d\\|\\d", coeff), paste0("\\hat\\theta_{",coeff,"}"), "\\beta")

  glue::glue("$({coef_symbol} = {beta}, CI = {conf_int})$")
}
