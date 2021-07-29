
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
#' @param table.envir Table environment to use
#' @param tabular.envir Tabular environment to use
#'
#' @return Formatted table
.summary_as_tex.brmsfit <- function(model,
                                    caption = NA,
                                    label = NA,
                                    include_prior = FALSE,
                                    table.envir = "table",
                                    tabular.envir = "tabular",
                                    ci_level = .95,
                                    use_ROPE = FALSE){
  outformat <- .get_knitr_table_type()

  coefs <- tidy_brmsfit(model, include_prior)
  ci_label <- paste0(ci_level * 100, "\\% CI")

  add_rope <- .process_rope(model, use_ROPE)
  use_ROPE <- use_ROPE | class(use_ROPE)[[1L]] == "rope"

  header_labels <- c("Term",
                     "Estimate",
                     "Est. Error",
                     ci_label)


  latex_table <- dplyr::mutate(coefs, term = gsub("_","\\\\_",term))

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
  latex_table <- gsub(r"(\{\})","",latex_table)
  latex_table <- gsub(r"(begin\{table\})", r"(begin\{table\}\[htbp\])", latex_table)
  .cleanup_latex_table(latex_table, table.envir, tabular.envir)
}

.process_rope <- function(model, use_ROPE) {

  if (class(use_ROPE)[[1L]] == "rope"){
    rope <- use_ROPE
    use_ROPE <- TRUE
  } else if (use_ROPE)
    rope <- bayestestR::rope(model)
  else
    add_rope <- NA

  if (use_ROPE){
    add_rope <- dplyr::transmute(dplyr::group_by(rope, Parameter),
                                 term = gsub("^b_", "", Parameter),
                                 pct = round(ROPE_Percentage * 100, 2))
    add_rope[["term"]] <- .fix_interaction_labels(use_ROPE, model)
    add_rope[['Parameter']] <- NULL
  }

  suppressWarnings(
    if (!is.na(add_rope))
      add_rope <- dplyr::mutate(add_rope, term = gsub("_","\\\\_",term))
  )

  add_rope
}
