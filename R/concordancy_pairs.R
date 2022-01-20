#' Calculate concordancy pairs
#'
#' Calcualtes the number of concordant and number of discordant pairs in
#' a 2D contingency table. Ascending or descending order does not matter for
#' this calculation. Modeled after Agresti 2010 pg 186. Generally, if there
#' are more concordant pairs than discordant pairs (C - D > 0), then the
#' association of the ordinal variables is positive. If more discordant
#' (C - D < 0), then the association is negative.
#'
#' @param contingency_table Contingency table of counts
#'
#' @return Named vector of number of Concordant and Discordant pairs.
#' @export
concordancy_pairs <- function(contingency_table) {
  k_rows = nrow(contingency_table)
  l_cols = ncol(contingency_table)

  C = 0
  D = 0
  for (k in seq_len(k_rows)) {
    for (i in seq_len(k)) {
      for (l in seq_len(l_cols)) {
        # Add to number of concordant pairs
        for (j in seq_len(l)) {
          C = C + contingency_table[i,j] * contingency_table[k,l]
        }
        # Add to number of discordant pairs; the first three parts are the same
        for (j in l:l_cols) {
          D = D + contingency_table[i,j] * contingency_table[k,l]
        }
      }
    }
  }

  c("Concordant" = C, "Discordant" = D)
}
