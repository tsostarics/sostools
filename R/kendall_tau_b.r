#' Kendall's tau-b
#'
#' Calculate sample form of Kendall's Tau-b (Kendall 1945; Agresti 2010, pg 188).
#'
#' Another association measure based on C-D that also incorporates information
#' about tied pairs. "In fact, tau-b-hat is a type of correlation even for rxc
#' tables, using sign socres for pairs of observations." (Agresti 2010, 188)
#'
#' Note that this is NOT the same as Kendall's tau (Kendall 1938), which should
#' be used for continuous variables where there are no tied pairs (Tx=Ty=Txy=0).
#'
#' Because this uses concordancy pairs, ascending or descending order in the
#' `xtabs` contingency table doesn't matter.
#'
#' @param contingency_table 2D contingency table of counts
#'
#' @return Kendall's tau b for the sample contingency table.
#' @export
kendall_tau_b <- function(contingency_table) {
  c_pairs <- concordancy_pairs(contingency_table)
  n <- sum(contingency_table)
  ties <- tied_pairs(contingency_table)

  (c_pairs[['Concordant']] - c_pairs[['Discordant']]) / (sqrt((n*(n-1)/2 - ties[['Tx']]) * (n*(n-1)/2 - ties[['Ty']])))
}


#' Number of tied pairs
#'
#' Calculate the number of tied pairs in a contingency table, see Agresti 2010
#' pg 188. Txy is not calculated because it factors out of other calculations.
#' This is used in the calculation of Kendall's tau-b
#'
#' @param contingency_table 2D Contingency table
#'
#' @return Named vector of the number of tied pairs Tx and Ty
tied_pairs <- function(contingency_table) {
  Tx <- rowSums(contingency_table) * (rowSums(contingency_table) - 1) / 2
  Ty <- colSums(contingency_table) * (colSums(contingency_table) - 1) / 2
  c("Tx" = sum(Tx), "Ty" = sum(Ty))
}
