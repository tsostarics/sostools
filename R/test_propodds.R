#' Proportional odds Testing
#'
#' Tests for proportional odds assumption in data. You can use as many predictors
#' as you want, but you cannot use random effects (only fixed effects allowed)
#' for the time being. A message will be displayed whether if the proportional odds
#' assumption is likely met or not. A dataframe with estimates and confidence intervals
#' will be returned if you want to plot th rsults to double check.
#'
#' @param ord_data Data frame with your data
#' @param model_formula The model formula to provide to a glm call
#' @param clm_obj Optional CLM or CLMM object to compare estimates to
#' @param .link Link function, defaults to logit for proportional odds model,
#' message is sent if this differs from clm_obj's link function
#' @param control Control to use for mixed models, defaults to
#'
#' @return A data frame with estimates and confidence intervals for each scale response
#' @export
test_propodds <- function(ord_data, model_formula, clm_obj = NULL, .link = "logit", control = list()) {
  if (!is.null(clm_obj)) {
    if (!class(clm_obj) %in% c('clmm','clm'))
      stop("clm_obj must be of class clm or clmm")
    if (clm_obj$link != .link)
      message(glue::glue("CLM link is {clm_obj$link} but .link is {.link}"))
  }

  requireNamespace("ordinal", quietly = TRUE)

  model_formula <- as.character(model_formula)

  # Check if the model is mixed (glmer) or not (glm) to use appropriate functions
  is_mixed <- grepl("\\|", model_formula[[3L]])
  if (is_mixed)
    requireNamespace("lme4", quietly = TRUE)
  mdl_fx <- ifelse(is_mixed, lme4::glmer, stats::glm)
  coef_fx <- ifelse(is_mixed, lme4::fixef, stats::coef)

  response_var <- model_formula[[2L]]
  responses <- as.numeric(levels(factor(ord_data[[response_var]])))
  results <-
    purrr::map_df(
      # Ensures the signs of the estimates are equivalent
      rev(responses[-1L]),
      function(j) .run_single_glm(ord_data,
                                  mdl_fx,
                                  coef_fx,
                                  model_formula,
                                  response_var,
                                  j,
                                  .link,
                                  control
      )
    )

  # Remove the random effects and intercepts terms from the checks
  term_names <- unique(results[['term']])
  term_names <- stringr::str_sort(term_names[!grepl("(Intercept)|\\.sig", term_names)])
  results <- dplyr::filter(results, term %in% term_names)

  # If a CLM object is passed, include the confidence regions from it
  if (!is.null(clm_obj)){
    propodds_checks <- .add_ordinal_result(clm_obj, results)
  } else {
    propodds_checks <-
      results |>
      dplyr::group_by(term) |>
      dplyr::mutate(check = ci_low < mean(estimate) && ci_high > mean(estimate))
  }

  n_fail <- sum(propodds_checks[['check']], na.rm = TRUE)


  # Print message to console
  issue_string <- glue::glue("Issues with {n_fail}/{nrow(propodds_checks)}")
  if (n_fail / nrow(propodds_checks) < .1)
    message(crayon::green(glue::glue("Proportional Odds assumption likely holds. {issue_string}.")))
  else
    message(crayon::red(glue::glue("Proportional Odds assumption might not hold. {issue_string}.")))

  propodds_checks
}

#' Merge CLMM object
#'
#' Adds the confidence intervals for the CLM estimated effects to compare the
#' individual binary regressions against. Note that interactions must be
#' specified in the same order in both model equations. Will check if the
#' confidence intervals from the binary regressions include the confidence
#' region from the CLM/CLMM estimates.
#'
#' @param clm_obj CLM or CLMM object from the ordinal package
#' @param results Results from .run_single_glm
#'
#' @return Extended dataframe
.add_ordinal_result <- function(clm_obj, results) {
  estimates <-
    broom::tidy(clm_obj)[,c('term','estimate')] |>
    `colnames<-`(c('term','clm_estimate'))

  confidence_intervals <-
    confint(clm_obj, level = .99) |>
    as_tibble(rownames = "term") |>
    `colnames<-`(c('term',"clm_low","clm_high"))

  results |>
    dplyr::left_join(merge(estimates, confidence_intervals)) |>
    dplyr::group_by(term) |>
    dplyr::mutate(check = ifelse(ci_low > clm_high | ci_high < clm_low, 1, 0))
}

#' Run binary regression
#'
#' Split data at cutpoint j and run a binary regression with the given
#' link function.
#'
#' @param ord_data Ordinal dataframe
#' @param mdl_fx Model function to use, either glm or glmer
#' @param coef_fx Coefficient function to use, either fixef or coef
#' @param model_formula Model formula to evaluate
#' @param response_var LHS of the formula
#' @param j Cutpoint value
#' @param .link Link function, default is logit (ie logistic regression)
#' @param control Control to use for mixed models, will default to bobyqa if not specified
#'
#' @return Dataframe with binary regression results
.run_single_glm <- function(ord_data, mdl_fx, coef_fx, model_formula, response_var, j, .link = "logit", control=list()) {
  # Recode values below threshold to 1, above threshold to 0, update model formula
  new_data <- dplyr::mutate(ord_data, j_response = as.numeric(!!sym(response_var) >= j))
  model_formula[[2L]] <- "j_response"

  # Concatenate formula to use for this run
  glm_formula <- stats::formula(paste(model_formula[[2L]],"~",model_formula[[3L]]))

  # If this is a mixed model and control is not specified, use bobyqa
  if (identical(control, list()) & identical(mdl_fx, lme4::glmer))
    control <- lme4::glmerControl("bobyqa")

  mdl <- mdl_fx(glm_formula, family = binomial(.link), data = new_data, control = control)

  # Check for successful convergence with code 0, method is differs for glm & glmer
  conv_code <- 0
  if (identical(mdl_fx, stats::glm))
    conv_code <- ifelse(mdl$converged, 0, 1)
  else if (!identical(mdl@optinfo$conv$lme4, list()))
    conv_code <- ifelse(is.null(mdl@optinfo$conv$lme4$code), 1, mdl@optinfo$conv$lme4$code)

  # Get 99% wald confidence intervals and return results
  ci <- stats::confint(mdl, method = "Wald",level = .99)
  ci <- ci[!is.na(ci[,1]) | !is.na(ci[,2]),] # remove any random effects
  tibble::tibble(!!response_var := j,
                 term = names(coef_fx(mdl)),
                 estimate = coef_fx(mdl),
                 ci_low = ci[,1L],
                 ci_high = ci[,2L],
                 converged = conv_code)
}


#' Plot proportional odds output
#'
#' A very simple helper to plot the estimates from the proportional odds tester.
#'
#' @param propodds_results Output of test_propodds
#'
#' @export
plot_propodds <- function(propodds_results) {
  resp_var <- colnames(tst)[1L]
  clm_region <- NULL
  if ('clm_high' %in% colnames(propodds_results))
    clm_region <- ggplot2::geom_ribbon(aes(x = !!sym(resp_var),
                                           ymax = clm_high,
                                           ymin = clm_low),
                                       fill = "#6edbbe",
                                       alpha=.4,
                                       inherit.aes = FALSE)

  requireNamespace("ggplot2", quietly = TRUE)
  propodds_results |>
    dplyr::filter(term != '(Intercept)') |>
    dplyr::group_by(term) |>
    dplyr::mutate(check = ifelse(check == 1, "red","black"),
                  converged = factor(converged)) |>
    ggplot2::ggplot(aes(x = !!sym(resp_var), y = estimate, ymax = ci_high, ymin = ci_low, color = I(check), shape = converged)) +
    clm_region +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar() +
    # ggplot2::geom_hline(aes(yintercept = mn), linetype = "dashed") +
    ggplot2::facet_wrap(~term) +
    ggplot2::theme_minimal()
}
