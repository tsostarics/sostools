#' Model Summary as table
#'
#' This is just a quick way to make a table for output to either html or, more
#' importantly, latex fragments. If you want more fine grained customization,
#' use gtsummary or kable yourself. Right now these have to be glmers or lmers,
#' but I'll make something more general later when/if I need it.
#'
#' @param model Model object
#' @param statistic Header label for the test statistic, recommended to bookend
#' in $s
#' @param caption Caption to use for the table
#' @param label Label to use when referencing the table
#'
#' @export
summary_as_tex <- function(model, statistic = "$z$", caption = NA, label = NA){
  requireNamespace("knitr", quietly = TRUE)
  requireNamespace("scales", quietly = TRUE)
  if (knitr::is_html_output())
    outformat  <-  "pipe"
  else
    outformat <- "latex"
  model %>%
    broom.mixed::tidy() %>%
    dplyr:: mutate(p.value = scales::pvalue(p.value)) %>%
    dplyr::filter(is.na(group)) %>% # note this will cause issues for non mixed models
    dplyr::select(-effect, -group) %>%
    knitr::kable(digits = c(0,2,2,2,3),
                 col.names = c("Term",
                               "Estimate",
                               "SE",
                               statistic,
                               "$p$"),
                 caption = caption,
                 escape = FALSE,
                 format = outformat,
                 booktabs = TRUE,
                 longtable = FALSE,
                 table.envir = "table",
                 label = label)
}
