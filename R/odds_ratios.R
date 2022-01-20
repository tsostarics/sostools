#' Calculate odds ratios from contingency table
#'
#' Calculates either local, cumulative, or global odds ratios for a given
#' contingency table (best made with `xtabs`). Follows the general form equation
#' given in Agresti, 2010 pg 23. Can also support a table of probabilities rather
#' than counts so long as `n` is provided.
#'
#' Note that the interpretation of the resulting `odds_ratio` index changes
#' depending on the ordering of the factors. `xtabs` will use increasing order
#' (highest X on the right) while Agresti's textbook uses decreasing order
#' (highest X on the left). The signs of the estimates are the same regardless
#' of the order, though. If you would like them to be reversed, use the `reverse`
#' argument.
#'
#' 95% wald intervals can also be calculated for each logit (Agrsti 2010 pg 27),
#' and Bonferroni corrections to the standard errors are also supported.
#'
#' @param table Contingency table, recommended to use `xtabs`
#' @param type String of which logit to return, either cumulative (default), local, or global
#' @param n If using probabilities instead of counts, the total number of observations
#' @param use_confint Logical, default TRUE, whether 95% wald confidence intervals
#' should be calculated and reported in a table
#' @param bonf Logical, default FALSE, whether to use Bonferroni correction on standard errors
#' @param reverse Logical, default FALSE, whether to reverse the table to be
#' descending rather than ascending.
#'
#' @return data.frame with odds ratios, standard errors, and confidence intervals
#' @export
odds_ratios <- function(table,
                        type = 'cumulative',
                        n = NA,
                        use_confint = TRUE,
                        bonf = FALSE,
                        reverse = FALSE) {
  stopifnot(type %in% c('local','cumulative','global'))
  stopifnot(is.matrix(table))
  # If counts are given
  if (!all(table < 1)) {
    # Throw error if there are any non-whole numbers
    stopifnot("must provide whole numbers if not providing probabilities" = all(table == round(table)))
    if (is.na(n))
      n <- sum(table)
    table <- table / sum(table)
  } else {
    # If proportions are given without n
    if (use_confint & is.na(n)) {
      warning("Cannot calculate SE and confidence intervals without n")
    }
    use_confint <- FALSE
  }

  if (reverse)
    table <- matrix(rev(table), nrow = nrow(table))

  r <- nrow(table)
  c <- ncol(table)
  i_indices <- seq_len(r - 1)
  j_indices <- seq_len(c - 1)

  odds_ratio_matrix     <- matrix(nrow = r-1, ncol = c-1)
  standard_error_matrix <- matrix(nrow = r-1, ncol = c-1)

  for (i in i_indices) {
    for (j in j_indices) {

      if (type == 'local') {
        lambda_ij   <- table[i,j]
        lambda_ij1  <- table[i,j+1]
        lambda_i1j  <- table[i+1,j]
        lambda_i1j1 <- table[i+1,j+1]
      } else if (type == 'cumulative') {
        lambda_ij   <- sum(table[i, 1:j])
        lambda_ij1  <- sum(table[i, (j+1):c])
        lambda_i1j  <- sum(table[i+1,1:j])
        lambda_i1j1 <- sum(table[i+1, (j+1):c])
      } else { # Global
        lambda_ij   <- sum(table[1:i, 1:j])
        lambda_ij1  <-  sum(table[1:i, (j+1):c])
        lambda_i1j  <- sum(table[(i+1):r, 1:j])
        lambda_i1j1 <- sum(table[(i+1):r, (j+1):c])
      }

      odds_ratio_matrix[i,j] <- (lambda_ij * lambda_i1j1) / (lambda_i1j * lambda_ij1)
      if (use_confint)
        standard_error_matrix[i,j] <- sqrt(sum(1/(c(lambda_ij, lambda_i1j, lambda_ij1, lambda_i1j1) * n)))
    }
  }
  if (use_confint)
    return(.generate_or_table(r, c, odds_ratio_matrix, standard_error_matrix, bonf))
  odds_ratio_matrix
}


#' Generate odds ratio table
#'
#' Given an odds ratio matrix and its associated standard errors, calculate
#' confidence intervals and report in a table like Agresti 2010 pg 29
#' (Table 2.6)
#'
#' @param r Number of rows
#' @param c Number of columns
#' @param odds_ratio_matrix Odds ratios
#' @param standard_error_matrix Standard errors
#' @param bonf Logical, whether bonferroni correction should be performed
.generate_or_table <- function(r, c, odds_ratio_matrix, standard_error_matrix, bonf) {
  ratio_names <- expand.grid(seq_len(r-1), seq_len(c-1))
  ratio_names <- paste(ratio_names[['Var1']], ratio_names[['Var2']], sep = ",")
  odds_ratio_table <-
    data.frame(odds_ratio     = ratio_names,
               sample_value   = as.vector(odds_ratio_matrix),
               log_odds_ratio = log(as.vector(odds_ratio_matrix)),
               SE             = as.vector(standard_error_matrix))
  z_val <- ifelse(bonf, -qnorm(.05 / nrow(odds_ratio_table) / 2), 1.96)
  odds_ratio_table[['confint.low']]  <- odds_ratio_table[['sample_value']] * exp(-z_val * odds_ratio_table[['SE']])
  odds_ratio_table[['confint.high']] <- odds_ratio_table[['sample_value']] * exp(z_val * odds_ratio_table[['SE']])
  odds_ratio_table[['log.confint.low']] <- log(odds_ratio_table[['confint.low']])
  odds_ratio_table[['log.confint.high']] <- log(odds_ratio_table[['confint.high']])
  odds_ratio_table
}
