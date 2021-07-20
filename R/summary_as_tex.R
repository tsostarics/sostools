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
#'
#' @export
#' @importFrom stats p.adjust
summary_as_tex <- function(model,
                           correct=NA,
                           statistic = "$z$",
                           caption = NA,
                           label = NA,
                           include_prior = FALSE,
                           ci_level = .95,
                           use_ROPE = FALSE){
  requireNamespace("knitr", quietly = TRUE)
  requireNamespace("scales", quietly = TRUE)

  if (class(model)[[1L]] == "clmm")
    return(.summary_as_tex.clmm(model, correct, statistic, caption, label))

  if (class(model)[[1L]] == 'brmsfit')
    return(.summary_as_tex.brmsfit(model, caption, label, include_prior, ci_level, use_ROPE))
  tidy_fx <- .get_tidy_fx(model)

  if (knitr::is_html_output())
    outformat  <-  "pipe"
  else
    outformat <- "latex"

  coefs <- tidy_fx(model)

  if (any(!is.na(correct)))
    coefs <- .adjust_pvals(coefs, correct)

  summary_table <-
    coefs |>
    dplyr::mutate(p.value = scales::pvalue(p.value),
                  term = gsub("_","\\\\_",term))
  if ("group" %in% names(summary_table)){
    summary_table <-
      summary_table |>
      dplyr::filter(is.na(group)) |> # note this will cause issues for non mixed models
    dplyr::select(-effect, -group)
    }

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
}

# summary_as_tex.lm <- function(model, statistic = "$t$", caption = NA, label = NA){
#
# }

#' BRMS Table output
#'
#' @param model brmsfit object
#' @param caption Caption for LaTeX table
#' @param label Label for LaTeX cross referencing
#' @param include_prior Logical, should priors be included in table?
#' @param ci_level Defaults to .95
#' @param use_ROPE ROPE object from bayestestR::rope, or TRUE to run this with the
#' default settings. Adds the % in ROPE to table after confidence intervals. FALSE
#' to not include.
#'
#' @return Formatted table
.summary_as_tex.brmsfit <- function(model, caption = NA, label = NA, include_prior = FALSE, ci_level = .95, use_ROPE = FALSE){
  if (knitr::is_html_output())
    outformat  <-  "pipe"
  else
    outformat <- "latex"

  coefs <- tidy_brmsfit(model, include_prior)
  ci_label <- paste0(ci_level * 100, "\\% CI")

  if (class(use_ROPE)[[1L]] == "rope"){
    add_rope <- dplyr::transmute(use_ROPE |> dplyr::group_by(Parameter),
                                 term = gsub("^b_", "", Parameter),
                                 pct = round(ROPE_Percentage * 100, 2))
    add_rope[["term"]] <- .fix_interaction_labels(use_ROPE, model)
    add_rope[['Parameter']] <- NULL
    use_ROPE <- TRUE
  } else if (use_ROPE){
    requireNamespace('bayestestR', quietly = TRUE)
    add_rope <- dplyr::transmute(bayestestR::rope(model) |> dplyr::group_by(Parameter),
                                 term = gsub("^b_", "", Parameter),
                                 pct = round(ROPE_Percentage * 100, 2))
    add_rope[["term"]] <- .fix_interaction_labels(use_ROPE, model)
    add_rope[['Parameter']] <- NULL

  } else {
    add_rope <- NA
    use_ROPE <- FALSE
  }
  header_labels <- c("Term",
                     "Estimate",
                     "Est. Error",
                     ci_label)


  latex_table <- dplyr::mutate(coefs, term = gsub("_","\\\\_",term))
  suppressWarnings(
    if (!is.na(add_rope))
    add_rope <- dplyr::mutate(add_rope, term = gsub("_","\\\\_",term))
  )
  digits <- c(0,2,2,0)
  if (use_ROPE){
    latex_table <-
      dplyr::left_join(latex_table, add_rope, by = 'term', copy = TRUE) |>
      dplyr::relocate(pct, .after = 'confint')
    header_labels <- c(header_labels, "\\% in ROPE")
    digits <- c(digits,2)
  }
  if (include_prior){
    header_labels <- c(header_labels, "Prior")
    digits <- c(digits,0)
  }
  latex_table <- knitr::kable(latex_table,
                              digits = digits,
                              col.names = header_labels,
                              caption = caption,
                              escape = FALSE,
                              format = outformat,
                              booktabs = TRUE,
                              longtable = FALSE,
                              table.envir = "table",
                              label = label)

  gsub(r"(\{\})","",latex_table)

}

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

.summary_as_tex.clmm <- function(model, correct, statistic = "$z$", caption = NA, label = NA) {

  if (knitr::is_html_output())
    outformat  <-  "pipe"
  else
    outformat <- "latex"

  coefs <- broom::tidy(model) # might need to change

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


.get_tidy_fx <- function(model) {
  if (class(model)[[1]] %in% c('lmerMod','glmerMod'))
    return(broom.mixed::tidy)

  return(broom::tidy)
}

