#' Fit GAMM with diagnostic checks
#'
#' Runs `mgcv::bam` but also saves the result to a file and runs diagnostics
#' to check if the model specification or data has changed since the last time
#' it was fit.
#'
#' @param mdl_formula Formula passed to `bam`
#' @param file Path to a file to save the model to
#' @param data Dataframe to use
#' @param ... Additional arguments passed to `bam` (method, nthreads, etc.)
#'
#' @returns the result of `bam`
#' @export
#'
#' @examples
#' \dontrun{
#' gam_mdl <- fit_bam(
#' y ~ grp +
#'      s(t, grp, bs = "fs", k = 15) +
#'      s(t, grp_2, bs = "fs", by = grp, k = 15),
#' "Models/gam_mdl.rds",
#' data = mdl_data,
#' discrete = TRUE,
#' method = "fREML",
#' nthreads = 8
#' )
#'
#' }
fit_bam <- function(mdl_formula,
                    file,
                    data,
                    ...) {
  rlang::is_installed("mgcv")
  call_q <- rlang::enquo(mdl_formula)

  call_expr <- rlang::quo_get_expr(call_q)
  expanded_call <- match.call(definition = rlang::eval_tidy(call_expr[[1]]), call = call_expr)

  if (!"formula" %in% names(expanded_call))
    stop("formula must be provided in model function call")

  fx <- expanded_call[["formula"]]

  # If the formula is held in another variable, we need to retrieve the value
  # and verify that it's a formula
  if (rlang::is_symbol(fx)) {
    fx <- rlang::eval_tidy(expanded_call[["formula"]])
    if (!rlang::is_formula(fx))
      stop(paste0(deparse1(expanded_call[["formula"]]), " is not a formula"))
  }


  if (!grepl("rds$", file))
    stop("Filename must use .rds extension")

  mdl <- NULL
  must_refit <-  FALSE

  filepath <- file

  if (file.exists(filepath)) {
    mdl <- readRDS(filepath)

    required_variables <- colnames(mdl[["model"]])

    same_formula <-
      identical(as.character(mdl[["call"]][["formula"]]),
                as.character(fx),
                ignore.environment = TRUE)

    if (!same_formula) {
      previous_formula <-
        paste0(trimws(format(mdl[["call"]][["formula"]])), collapse = " ")

      new_formula <-
        paste0(trimws(format(fx)), collapse = " ")

      cli::cli_alert_warning(paste0("Formula does not match previous formula.\nOld: ",
                                    previous_formula,
                                    "\nNew: ",
                                    new_formula))
      must_refit <- TRUE
    } else {
      cli::cli_alert_success("Formula matches")
      if (!identical(dim(data[required_variables]), dim(mdl[["model"]]))) {
        cli::cli_alert_warning("Data dimensions have changed")
        must_refit <- TRUE

      } else {
        cli::cli_alert_success("Data has same dimensions")

        cols_changed <-
          vapply(required_variables,
                 \(x) any(data[[x]] != mdl[["model"]][[x]]),
                 logical(1))

        if (any(cols_changed)) {
          cli::cli_alert_warning(paste0("Columns in data have changed: ",
                                        paste0(required_variables[cols_changed], collapse = ", ")))
          must_refit <- TRUE
        } else {
          cli::cli_alert_success("Data is unchanged")
        }

      }
    }

    previous_call_args <- names(mdl[["call"]])
    previous_call_args <- previous_call_args[!previous_call_args %in% c("", "formula", "data")]

    dots <- rlang::dots_list(...)

    dots_names <- names(dots)
    new_args <- !dots_names %in% previous_call_args

    if (any(new_args)) {
      cli::cli_alert_warning("New arguments provided")
      must_refit <- TRUE
    } else {
      cli::cli_alert_success("No new arguments detected")
      if (!all(dots_names %in% previous_call_args)) {
        cli::cli_alert_warning("Missing arguments")
        must_refit <- TRUE
      } else {
        cli::cli_alert_success("No missing arguments")
      }
    }

  } else {
    cli::cli_alert_warning("File not found, assuming new model to be fit")
    must_refit <- TRUE
  }

  if (must_refit) {
    cli::cli_alert_info(paste0("Fitting model at ", Sys.time()))
    mdl <- do.call(mgcv::bam, args = list(formula = fx, data = data, ... = ...))
    saveRDS(mdl, filepath)
    cli::cli_alert_success(paste0("Model saved to ", file, " at ", Sys.time()))
  } else {
    cli::cli_alert_success("Model has not changed, loading from disk")
  }

  mdl
}
