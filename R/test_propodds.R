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
#'
#' @return A data frame with estimates and confidence intervals for each scale response
#' @export
test_propodds <- function(ord_data, model_formula) {
  requireNamespace("ordinal", quietly = TRUE)
  model_formula <- as.character(model_formula)
  is_mixed <- grepl("\\|", model_formula[[3L]])
  if (is_mixed)
    requireNamespace("lme4", quietly = TRUE)
  mdl_fx <- ifelse(is_mixed, lme4::glmer, stats::glm)
  coef_fx <- ifelse(is_mixed, lme4::fixef, stats::coef)
  response_var <- model_formula[[2L]]
  responses <- as.numeric(levels(factor(ord_data[[response_var]])))
  results <-
    purrr::map_df(
      rev(responses)[-1L],
      function(j) .run_single_glm(ord_data,
                                  mdl_fx,
                                  coef_fx,
                                  model_formula,
                                  response_var,
                                  j
      )
    )

  # Remove the random effects and intercepts terms from the checks
  term_names <- unique(results[['term']])
  term_names <- stringr::str_sort(term_names[!grepl("(Intercept)|\\.sig", term_names)])

  # Check if confidence intervals are far from the mean for each term
  propodds_checks <-
    results |>
    dplyr::filter(term %in% term_names) |>
    dplyr::group_by(term) |>
    dplyr::mutate(mn = mean(estimate),
                  check = ifelse(ci_low > mn | ci_high < mn, 1, 0))
  n_fail <- sum(propodds_checks$check, na.rm = T)


  # Print message to console
  issue_string <- glue::glue("Issues with {n_fail}/{nrow(propodds_checks)}")
  if (n_fail / nrow(propodds_checks) < .1)
    message(crayon::green(glue::glue("Proportional Odds assumption likely holds. {issue_string}.")))
  else
    message(crayon::red(glue::glue("Proportional Odds assumption might not hold. {issue_string}.")))

  results
}

.run_single_glm <- function(ord_data, mdl_fx, coef_fx, model_formula, response_var, j) {
  # Recode values below threshold to 1, above threshold to 0, update model formula
  new_data <- dplyr::mutate(ord_data, j_response = as.numeric(scale_response <= j))
  model_formula[[2L]] <- "j_response"
  glm_formula <- stats::formula(paste(model_formula[[2L]],"~",model_formula[[3L]]))
  mdl <- mdl_fx(glm_formula, family = "binomial", data = new_data)
  ci <- stats::confint(mdl, method = "Wald",level = .99)
  ci <- ci[!is.na(ci[,1]) | !is.na(ci[,2]),] # remove any random effects
  tibble::tibble(!!response_var := j,
         term = names(coef_fx(mdl)),
         estimate = coef_fx(mdl),
         ci_low = ci[,1L],
         ci_high = ci[,2L])
}

#' Plot proportional odds output
#'
#' A very simple helper to plot the estimates from the proportional odds tester.
#'
#' @param propodds_results Output of test_propodds
#' @param resp_var name of response variable
#'
#' @export
plot_propodds <- function(propodds_results, resp_var = "scale_response") {
  requireNamespace("ggplot2", quietly = TRUE)
  propodds_results |>
    dplyr::filter(term != '(Intercept)') |>
    dplyr::group_by(term) |>
    dplyr::mutate(mn = mean(estimate, na.rm = TRUE),
                  check = ifelse(ci_low > mn | ci_high < mn, "red", "black")) |>
    ggplot2::ggplot(aes(x = scale_response, y = estimate, ymax = ci_high, ymin = ci_low, color = I(check))) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar() +
    ggplot2::geom_hline(aes(yintercept = mn), linetype = "dashed") +
    ggplot2::facet_wrap(~term) +
    ggplot2::theme_minimal()
}
