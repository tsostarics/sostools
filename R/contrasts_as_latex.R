#' Make latex table of contrasts
#'
#' Given a factor column, print the contrast matrices used
#'
#' @param factor_col A factor column, eg data$gender
#'
#' @return A latex table string
#' @export
contrasts_as_latex <- function(factor_col) {
  requireNamespace("MASS", quietly = TRUE)
  contrast_matrix <- contrasts(factor_col)
  # Add this functionality in later, i don't feel like doing it rn bc i have to mess around with the dimnames
  # if (hypotheses)
  #   contrast_matrix <- .contrasts_to_hypotheses(contrast_matrix, nrow(contrast_matrix))
  value_matrix <- as.character(MASS::fractions(contrast_matrix))
  value_matrix <- apply(value_matrix, c(1,2), .as_latex_frac)
  n_cols <- ncol(value_matrix)
  n_rows <- nrow(value_matrix)
  header_row <- paste0(paste0(c(" ", colnames(value_matrix)),
                              collapse = " & "),
                       " \\\\\\\\")
  value_rows <- vapply(seq_len(n_rows),
                       function(x)
                       paste0(c(rownames(value_matrix)[x],
                                value_matrix[x,]),
                              collapse = "\t&\t"),
                       "char")
  value_rows <- paste(value_rows, collapse =  "\\\\\\\\\n")
  alignment <- paste0(c("l|", rep("c", n_cols)), collapse = "")

  latex_table <- "
  \\begin{table}[htb]
  \\centering
  \\begin{tabular}{ALIGNMENT}
  HEADER
  \\hline
  VALUES
  \\end{tabular}
  \\end{table}"

  latex_table <- gsub("ALIGNMENT", alignment, latex_table)
  latex_table <- gsub("HEADER", header_row, latex_table)
  latex_table <- gsub("VALUES", value_rows, latex_table)
  latex_table
}

.as_latex_frac <- function(fraction_string){
  requireNamespace("stringr", quietly = TRUE)
  if (!grepl("/",fraction_string))
    return(fraction_string)

  frac_values <- stringr::str_match_all(fraction_string, "(-?)(\\d)/(\\d)")[[1]][,2:4]

  paste0(frac_values[1],"$\\\\frac\\{",frac_values[2],"\\}\\{",frac_values[3],"\\}$")
}
