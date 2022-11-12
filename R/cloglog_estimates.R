#' Sample cloglog estimates
#'
#' Probit analog to `sostools::odds_ratios`. Given a 2d contingency table,
#' calculate the cloglog estimates between a reference level (row) and the
#' other rows. Does not currently support confidence intervals.
#'
#' @param table Contingency table from `xtabs`
#' @param reference Which row should be used as the reference level, defaults to
#' 1
#'
#' @return Data frame with cloglog thresholds and effect estimates.
#' @export
cloglog_estimates <- function(table, reference = 1) {
  cloglog_values <-
    apply(table, 1, function(row) cumsum(row / sum(row))) |>
    t() |>
    {\(pii) log(-log(1-pii))}()
  cloglog_values <- cloglog_values[,seq_len(ncol(cloglog_values) - 1)]

  row_indices <- seq_len(nrow(cloglog_values))
  row_indices <- row_indices[row_indices != reference]

  estimates <-
    sapply(row_indices,
           function(i) {
             apply(cloglog_values[c(reference,i),], 2L, base::diff)
           }
    )

  data.frame(effect = c(sapply(row_indices,
                               \(i)
                               paste0(i,"-",reference,",",seq_len(nrow(estimates)))
  )
  ),
  threshold = cloglog_values[reference,],
  effect_estimate = c(estimates)
  )
}
