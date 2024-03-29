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
#' @param include_prior Should priors be included? (only if model is brmsfit)
#' @param ci_level Defaults to .95 (only if model is brmsfit)
#' @param use_ROPE ROPE object from bayestestR::rope, or TRUE to run this with the
#' default settings. Adds the % in ROPE to table after confidence intervals. FALSE
#' to not include. (only if model is brmsfit)
#' @param table.envir LaTeX table environment to use, defaults to table, change
#' not implemented yet
#' @param tabular.envir LaTeX tabular environment to use, defaults to table, can
#' be changed to tabular*
#'
#' @export
#' @importFrom stats p.adjust
summary_as_tex <- function(model,
                           correct=NA,
                           statistic = "$z$",
                           caption = NA,
                           label = NA,
                           table.envir = "table",
                           tabular.envir = "tabular",
                           include_prior = FALSE,
                           ci_level = .95,
                           use_ROPE = FALSE){
  requireNamespace("knitr", quietly = TRUE)
  requireNamespace("scales", quietly = TRUE)

  change_call <- match.call()
  if (class(model)[[1L]] == "clmm"){
    # I really need to learn to implement usemethod honestly
    change_call[[1]] <- sym(".summary_as_tex.clmm")
    return(eval(change_call))
  }

  if (class(model)[[1L]] == 'brmsfit') {
    change_call[[1]] <- sym(".summary_as_tex.clmm")
    return(eval(change_call))
  }

  tidy_fx <- .get_tidy_fx(model)
  outformat <- .get_knitr_table_type()

  coefs <- tidy_fx(model)

  if (any(!is.na(correct)))
    coefs <- .adjust_pvals(coefs, correct)

  summary_table <-
    coefs |>
    dplyr::mutate(p.value = format_tbl_pvals(p.value),
                  term = gsub("_","\\\\_",term))
  if ("group" %in% names(summary_table)){
    summary_table <-
      summary_table |>
      dplyr::filter(is.na(group)) |> # note this will cause issues for non mixed models
      dplyr::select(-effect, -group)
  }

  latex_table <-
    summary_table |>
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

  .cleanup_latex_table(latex_table, table.envir, tabular.envir)

}

.get_knitr_table_type <- function() {
  if (knitr::is_html_output())
    return("pipe")
  else
    return("latex")
}

.cleanup_latex_table <- function(latex_table, table.envir, tabular.envir) {
  latex_table <- gsub(r"(begin\{table\})", r"(begin\{table\}\[htbp\])", latex_table)

  if (tabular.envir == "tabular*"){
    latex_table <- gsub(r"(begin\{tabular\}\[t\]\{l)", r"(begin\{tabular*\}\{\\textwidth\}\{l@\{\\extracolsep\{\\fill\}\})", latex_table)
    latex_table <- gsub(r"(end\{tabular\})",r"(end\{tabular*\})", latex_table)
  }

  note <- "\\\\flushleft
*p\\\\textless{}0.05; **p\\\\textless{}0.01; ***p\\\\textless{}0.001
\\\\end\\{table\\}"

  latex_table <- gsub(r"(\\end\{table\})", note, latex_table)
  latex_table
}

# summary_as_tex.lm <- function(model, statistic = "$t$", caption = NA, label = NA){
#
# }

.fix_interaction_labels <- function(rope, model){
  rope_terms <- gsub("^b_", "", rope[['Parameter']])
  model_terms <- rownames(brms::fixef(model))
  which_match <- rope_terms %in% model_terms
  model_terms_dots <- gsub(":", ".", model_terms)
  which_match_dots <- rope_terms %in% model_terms_dots

  if (all(which_match == which_match_dots))
    return(rope_terms)

  if (all(rope_terms[!which_match] == model_terms_dots[!which_match]))
    return(model_terms)
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


.get_tidy_fx <- function(model) {
  if (class(model)[[1]] %in% c('lmerMod','glmerMod'))
    return(broom.mixed::tidy)

  return(broom::tidy)
}

