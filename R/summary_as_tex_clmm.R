.summary_as_tex.clmm <- function(model,
                                 correct = NA,
                                 statistic = "$z$",
                                 table.envir = "table",
                                 tabular.envir = "tabular",
                                 caption = NA,
                                 label = NA) {
  requireNamespace("ordinal", quietly = TRUE)
  outformat <- .get_knitr_table_type()

  coefs <- broom::tidy(model) # might need to change

  if (any(!is.na(correct)))
    coefs <- .adjust_pvals(coefs, correct)

  latex_table <-
    coefs |>
    dplyr::mutate(p.value = scales::pvalue(p.value),
                  term = gsub("_","\\\\_",term),
                  term = ifelse(coef.type == 'intercept',
                                paste0("$\\theta_{",term,"}$"),
                                term)
    ) |>
    dplyr::select(-coef.type) |>
    knitr::kable(digits = c(0,2,2,2,0),
                 col.names = c("Term",
                               "Estimate",
                               "SE",
                               "$z$",
                               "$p$"),
                 caption = caption,
                 escape = FALSE,
                 format = outformat,
                 booktabs = TRUE,
                 longtable = FALSE,
                 table.envir = "table",
                 label = label)

  .cleanup_latex_table(latex_table, table.envir, tabular.envir)
}
