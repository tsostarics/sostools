#' Calculate Somers' d
#'
#' "Somers (1962) proposed a measure ismilar to gamma and tau-b, but which treats
#' Y as a response variable and X as an explanatory variable." (Agresti 2010,
#' 189).
#'
#' @param contingency_table 2D contingency table
#'
#' @return Sample estimate of Somers' d
#' @export
somers_d <- function(contingency_table) {
  ties <- tied_pairs(contingency_table)
  c_pairs <- concordancy_pairs(contingency_table)
  n <- sum(contingency_table)

  (c_pairs[['Concordant']] - c_pairs[['Discordant']]) / (n*(n-1) / 2 - ties[['Tx']])
}
