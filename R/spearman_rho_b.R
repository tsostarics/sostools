#' Spearman's rho-b for rank scored contingency tables
#'
#' "For observations on continuous variables that are ranked on each variable,
#' Spearman's rho is the ordinary correlation applied to the rank scores.
#' Kendall (1970, p. 38) proposed an analog of Spearman's rho for contingency
#' tables. It is the ordinary correlation applied using the ridit scores. [...]
#' Since the ridit scores are a linear function of the midrakbns, rho-b-hat also
#' equals the correlation applied to the sample midrank scores. For 2x2 tables,
#' rho-b = tau-b and then both measures equal the ordinary correlation."
#' (Agresti 2010, pg 192).
#'
#' @param contingency_table 2D contingency table
#'
#' @return Sample estimate of spearman's rho-b
rho_b <- function(contingency_table) {
  ridits <- marginal_ridits(contingency_table)

  prob_table <- contingency_table / sum(contingency_table)
  r <- nrow(contingency_table)
  c <- ncol(contingency_table)


  row_marginal_probs <- rowSums(contingency_table) / sum(contingency_table)
  col_marginal_probs <- colSums(contingency_table) / sum(contingency_table)

  numerator <-
    sum(
      vapply(seq_len(r),
             function(i) {
               sum(
                 vapply(seq_len(c),
                        function(j)
                          (ridits[['aXi']][[i]] - .5)*(ridits[['aYj']][[j]] - .5)*prob_table[i,j],
                        1.0
                 )
               )
             },
             1.0)
    )

  sum_i <-
    sum(
      vapply(seq_len(r),
             function(i)
               (ridits[['aXi']][[i]] - .5)^2*row_marginal_probs[[i]],
             1.0
      )
    )

  sum_j <-
    sum(
      vapply(seq_len(c),
             function(j)
               (ridits[['aYj']][[j]] - .5)^2*col_marginal_probs[[j]],
             1.0
      )
    )

  numerator / sqrt(sum_i * sum_j)

}


#' Calculate marginal distribution ridits
#'
#' Given a 2D contingency table, calculate the ridit values for the marginal
#' distributions. This is used to calculate Spearman's rho for rank scored
#' contingency tables. See Agresti 2010, pg 192.
#'
#' @param contingency_table 2D contingency table
#'
#' @return List of marginal ridit values along X and Y
#' @export
marginal_ridits <- function(contingency_table) {
  row_marginal_probs <- rowSums(contingency_table) / sum(contingency_table)
  col_marginal_probs <- colSums(contingency_table) / sum(contingency_table)

  lapply(list('aXi' = row_marginal_probs,
              'aYj' = col_marginal_probs),
         function(marginal_probs) {
           vapply(seq_along(marginal_probs),
                  function(i) {
                    sum(marginal_probs[seq_len(i-1)]) + marginal_probs[[i]] / 2
                  },
                  1.0
           )
         }
  )

}
