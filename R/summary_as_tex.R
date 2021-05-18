#' Model Summary as table
#'
#' This is just a quick way to make a table for output to either html or, more
#' importantly, latex fragments. If you want more fine grained customization,
#' use gtsummary or kable yourself. Right now these have to be glmers or lmers,
#' but I'll make something more general later when/if I need it.
#'
#' @param model Model object
#' @param correct Regular expressions targeting coefficients to bonferroni correct
#' @param statistic Header label for the test statistic, recommended to bookend
#' in $s
#' @param caption Caption to use for the table
#' @param label Label to use when referencing the table
#'
#' @export
#' @importFrom stats p.adjust
summary_as_tex <- function(model, correct=NA, statistic = "$z$", caption = NA, label = NA){
  requireNamespace("knitr", quietly = TRUE)
  requireNamespace("scales", quietly = TRUE)

  if (class(model) == "clmm")
    return(.summary_as_tex.clmm(model, correct, statistic, caption, label))

  if (knitr::is_html_output())
    outformat  <-  "pipe"
  else
    outformat <- "latex"

  coefs <- broom::tidy(model)

  if (any(!is.na(correct)))
    coefs <- .adjust_pvals(coefs, correct)

  coefs |>
    dplyr::mutate(p.value = scales::pvalue(p.value),
                  term = gsub("_","\\\\_",term)) |>
    dplyr::filter(is.na(group)) |> # note this will cause issues for non mixed models
    dplyr::select(-effect, -group) |>
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

# summary_as_tex.lm <- function(model, statistic = "$t$", caption = NA, label = NA){
#
# }

.summary_as_tex.clmm <- function(model, correct, statistic = "$z$", caption = NA, label = NA) {

  if (knitr::is_html_output())
    outformat  <-  "pipe"
  else
    outformat <- "latex"

  coefs <- broom::tidy(model)

  if (any(!is.na(correct)))
    coefs <- .adjust_pvals(coefs, correct)

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
}

#' Add addlinespace commands to latex table
#'
#' Default output of latex tables will add some addlinespace commands for you,
#' but you can provide which terms you want to add the spaces after with this
#' command. Note that special characters like curly braces should be escaped
#' with two backslashes.
#'
#' @param table Latex table
#' @param terms Strings, which terms to add addlinespace after
#' @export
addlinespace <- function(table, terms) {
  requireNamespace("stringi", quietly = TRUE)
  if (!knitr::is_latex_output())
    return(table)

  line_regexes <-
    vapply(terms,
           function(x) paste0("(",x,".+\\\\)"),
           "char"
    )
  latex_table <- gsub("\n\\\\addlinespace","",table)
  for (line in line_regexes) {
    latex_table <- stringi::stri_replace_all_regex(latex_table, line, "$1\n\\\\addlinespace")
  }
  class(latex_table) <- "knitr_kable"
  attr(latex_table, "format") <- "latex"
  latex_table
}

