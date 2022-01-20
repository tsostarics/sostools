#' Calculate sample gamma-hat
#'
#' "Of the C+D pairs of observations that are concordant or discordant, the
#' proportion of C/(C+D) is concordant and the proportion D/(C+D) is discordant.
#' THe difference between these proportions is called gamma (Goodman and Kruskal,
#' 1954)" (Agresti 2010, pg 186).
#'
#' Calculates the sample gamma estimate, which takes values  -1 <= gamma <= 1.
#' Higher magnitude is stronger association, sign represents positive or negative
#' association.
#'
#' @param contingency_table 2D contingency table
#'
#' @return Gamma hat
#' @export
sample_gamma <- function(contingency_table) {
  c_pairs <- concordancy_pairs(contingency_table)
  (c_pairs[["Concordant"]] - c_pairs[["Discordant"]]) / (c_pairs[["Concordant"]] + c_pairs[["Discordant"]])
}
