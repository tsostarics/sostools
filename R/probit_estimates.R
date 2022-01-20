#' Sample probit estimates
#'
#' Probit analog to `sostools::odds_ratios`. Given a 2d contingency table,
#' calculate the probit estimates between a reference level (row) and the
#' other rows. Does not currently support confidence intervals.
#'
#' @param table Contingency table from `xtabs`
#' @param reference Which row should be used as the reference level, defaults to
#' 1
#'
#' @return Data frame with probit thresholds and effect estimates.
#' @export
probit_estimates <- function(table, reference = 1) {
  probits <-
    apply(table, 1, function(row) cumsum(row / sum(row))) |>
    t() |>
    qnorm()
  probits <- probits[,seq_len(ncol(probits) - 1)]

  row_indices <- seq_len(nrow(probits))
  row_indices <- row_indices[row_indices != reference]

  estimates <-
    sapply(row_indices,
           function(i) {
             apply(probits[c(reference,i),], 2L, base::diff)
           }
    )

  data.frame(effect = c(sapply(row_indices,
                               \(i)
                               paste0(i,"-",reference,",",seq_len(nrow(estimates)))
                               )
                        ),
             threshold = probits[reference,],
             effect_estimate = c(estimates)
  )
}
