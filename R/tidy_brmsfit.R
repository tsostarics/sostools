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
#' @param fix.intercept whether Intercept should be changed to (Intercept)
#' @param include_prior Should description of priors be included?
#' @param ... Other options to pass to broom.mixed::tidy

#' @return Tidy dataframe of fixed effects for use with other functions in this package
#'
#' @export
tidy_brmsfit <- function(model, include_prior = TRUE, fix.intercept = FALSE, ...) {
  if (class(model) != 'brmsfit') stop("non brmsfit object passed to tidy_brmsfit")
  requireNamespace("brms", quietly = TRUE)
  requireNamespace("broom.mixed", quietly = TRUE)

  which_effects <- "fixed"
  has_ranef <- nrow(model[['ranef']]) != 0L

  if (has_ranef)
    which_effects <- c(which_effects, "ran_pars", ...)

  mdl_summary <- broom.mixed::tidy(model,
                                   effects = which_effects,
                                   fix.intercept = fix.intercept)

  # Add confidence intervals
  mdl_summary <-
    dplyr::transmute(mdl_summary, term, estimate, est.error = std.error,
                     confint = paste0("[",
                                      round(conf.low, 2L),
                                      ";",
                                      round(conf.high, 2L),
                                      "]")

    )

  # Add priors as needed
  if (!include_prior)
    return(mdl_summary)

  priors <- .extract_priors(model)

  dplyr::inner_join(mdl_summary, priors, by = 'term')
}

.extract_priors <- function(model) {
  priors <- brms::prior_summary(model)
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
