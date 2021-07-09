#' Brmsfit fixed effects as tidy data frame
#'
#' Tidy helper for brmsfit objects
#'
#' Extracts fixed effects (class=b) and Intercept into a tidy data frame. You can
#' include the description of the priors used by setting include_prior to TRUE.
#' Note that I recommend that you specify all priors manually, if you set the default
#' for a class it should still work but no guarantees with more complex models.
#' Random effects currently not included, see broom.mixed::tidy for that
#' (that function doesn't work with models lacking random effects, go figure)
#'
#' @param model brmsfit object
#' @param include_prior Should description of priors be included?
#' @param probs probabilities for credible intervals, defaults to 95 percent
#'
#' @return Tidy dataframe of fixed effects for use with other functions in this package
#'
#' @export
tidy_brmsfit <- function(model, include_prior = TRUE, probs = c(0.025, 0.975)) {
  if (class(model) != 'brmsfit') stop("brmsfit object passed to tidy_brmsfit")
  requireNamespace("brms", quietly = TRUE)

  # Put fixed effects in tidy format
  mdl_summary <-
    tibble::as_tibble(
      brms::fixef(model, probs, robust = FALSE, summary = TRUE),
      rownames = "term"
    )
  colnames(mdl_summary) <- tolower(colnames(mdl_summary))

  ## Get the correct column names for the confidence interval bounds
  ci_lower_col <- paste0("q", probs[[1L]]*100)
  ci_upper_col <- paste0("q", probs[[2L]]*100)

  # Add confidence intervals
  mdl_summary <-
    dplyr::transmute(mdl_summary, term, estimate, est.error,
                     confint = paste0("[",
                                      round(!!sym(ci_lower_col), 2L),
                                      ";",
                                      round(!!sym(ci_upper_col), 2L),
                                      "]")

    )

  # Add priors as needed
  if (!include_prior)
    return(mdl_summary)

  priors <- .extract_priors(model)

  dplyr::inner_join(mdl_summary, priors, by = 'term')
}

.extract_priors <- function(model) {
  priors <- prior_summary(model)
  # When priors aren't manually specified they're underlyingly "" and inherit from the default
  default_b_prior <- priors[['prior']][priors[["coef"]] == "" & priors[["class"]] == "b"]
  priors[['prior']][priors[['class']] == 'b' & priors[['prior']] == ""] <- default_b_prior
  priors <-
    dplyr::ungroup(
      dplyr::transmute(
        dplyr::group_by(priors, '1'),
        term = ifelse(class == "b", coef, class),
        prior
      )
    )
  # Remove grouping factor
  priors[[1L]] <- NULL
  priors
}
