# +---------------------------+
# Date created:  2024-07-31
# Last modified: 2024-11-12
# Notable Dependencies:
#   brms >=2.21.6
#   diffobj
#   rstan
#   cmdstanr
# +---------------------------+
# requireNamespace("brms", versionCheck = list(version = "2.21.6"))
# requireNamespace("diffobj")
# requireNamespace("cmdstanr")
# requireNamespace("rstan")

# Some utilities for checking if a brmsfit object needs to be refit & if so, why.
# These essentially build off of `brm` and `brmsfit_needs_refit` for interactive
# use. Sometimes brms will refit or recompile a model and it's not clear why.
# `brmsfit_needs_refit` cannot be used on it's own, so you would have to
# manually debug it from within a `brm` call, which is annoying.



#' Determine potential reasons for refitting/recompilation
#'
#' Drop-in replacement for `brm()` that runs various diagnostics for why a
#' model might need to be refit.
#'
#' @details
#' This function runs the necessary code from `brm` to generate updated stan
#' code, which is then compared to the model saved in `file`.
#'
#' For interactive use, if you already have the saved model loaded in R, you
#' can pass this to `fit`. Otherwise, you can largely swap out the `brm` with
#' `diagnose_refit` as-is.
#'
#'
#' @param formula Formula for the model, will be compared to the cached model
#' @param data  Data to use for the for model, will be compared to the cached
#' model
#' @param fit brmsfit object if already loaded, otherwise use `file`. If both
#' are provided, `fit` is used instead to avoid loading the file again.
#' @param file Path to the file containing the model
#' @param verbose Whether to print detailed diagnostics, default TRUE
#' @inheritParams brms::brm
#' @return List of diagnostics, see `brmsfit_needs_refit2`
#'
#' @examples
#' \dontrun{
#'
#' # Example of dynamically changing the file_refit argument for a function
#' # that fits a generic model (intending to hold some hyperparameters the same
#' # across many models). Goal is to specify the model call only once, then
#' # only swap out things like the formula and data as needed.
#' fit_generic_model <- function(formula, filename, data=model_data, priors=base_model_priors) {
#'   # We need the full brm call, so we keep it in an expression.
#'   model_call <- expression(brm(formula = formula,
#'                                data = data,
#'                                family = lognormal(),
#'                                seed = 111,
#'                                cores = 4,
#'                                chains = 4,
#'                                threads = threading(3),
#'                                file = file.path(here::here("Models"), paste0(filename, ".rds")),
#'                                file_refit = "on_change",
#'                                iter = 10000,
#'                                warmup = 2000,
#'                                thin = 2,
#'                                silent = 0,
#'                                verbose = TRUE,
#'                                save_pars = save_pars(all = TRUE),
#'                                backend = 'cmdstanr',
#'                                prior = priors))
#'
#'   # Swap out brm with diagnose_refit; in interactive use we could have
#'   # just deleted brm and wrote diagnose_refit ourselves
#'   model_call[[1]][[1]] <- sym('diagnose_refit')
#'
#'   # Run diagnose_refit, determine whether we should refit
#'   should_refit <- determine_refit(eval(model_call))
#'
#'   # Will show never if it doesn't refit and always if it does refit
#'   message("Refit status for ", filename, ": ", should_refit)
#'
#'   # Swap back in brm, change the file_refit value
#'   model_call[[1]][[1]] <- sym('brm')
#'   model_call[[1]][["file_refit"]] <- should_refit
#'
#'   # Run the model
#'   mdl <- eval(model_call)
#'
#'   mdl
#' }
#' }
#'
diagnose_refit <- function(formula, data, family = stats::gaussian(), prior = NULL,
                           autocor = NULL, data2 = NULL, cov_ranef = NULL,
                           sample_prior = "no", sparse = NULL, knots = NULL,
                           drop_unused_levels = TRUE, stanvars = NULL, stan_funs = NULL,
                           fit = NULL, save_pars = getOption("brms.save_pars", NULL),
                           save_ranef = NULL, save_mevars = NULL, save_all_pars = NULL,
                           init = NULL, inits = NULL, chains = 4, iter = 2000,
                           warmup = floor(iter / 2), thin = 1,
                           cores = getOption("mc.cores", 1),
                           threads = getOption("brms.threads", NULL),
                           opencl = getOption("brms.opencl", NULL),
                           normalize = getOption("brms.normalize", TRUE),
                           control = NULL,
                           algorithm = getOption("brms.algorithm", "sampling"),
                           backend = getOption("brms.backend", "rstan"),
                           future = getOption("future", FALSE), silent = 1,
                           seed = NA, save_model = NULL, stan_model_args = list(),
                           file = NULL, file_compress = TRUE,
                           file_refit = getOption("brms.file_refit", "on_change"),
                           empty = FALSE, rename = TRUE, verbose = FALSE, ...) {


  # Bring in unexported functions
  brms_algorithm_choices   <- utils::getFromNamespace("algorithm_choices", "brms")
  brms_backend_choices     <- utils::getFromNamespace("backend_choices", "brms")
  brms_as_one_logical      <- utils::getFromNamespace("as_one_logical", "brms")
  brms_validate_silent     <- utils::getFromNamespace("validate_silent", "brms")
  brms_as_one_numeric      <- utils::getFromNamespace("as_one_numeric", "brms")
  brms_use_alias           <- utils::getFromNamespace("use_alias", "brms")
  brms_validate_threads    <- utils::getFromNamespace("validate_threads", "brms")
  brms_validate_opencl     <- utils::getFromNamespace("validate_opencl", "brms")
  brms_validate_formula    <- utils::getFromNamespace("validate_formula", "brms")
  brms_validate_data2      <- utils::getFromNamespace("validate_data2", "brms")
  brms_get_data2_autocor   <- utils::getFromNamespace("get_data2_autocor", "brms")
  brms_get_data2_cov_ranef <- utils::getFromNamespace("get_data2_cov_ranef", "brms")
  brms_validate_data       <- utils::getFromNamespace("validate_data", "brms")
  brms_substitute_name     <- utils::getFromNamespace("substitute_name", "brms")
  brms_brmsframe           <- utils::getFromNamespace("brmsframe", "brms")
  brms_.validate_prior     <- utils::getFromNamespace(".validate_prior", "brms")
  brms_validate_stanvars   <- utils::getFromNamespace("validate_stanvars", "brms")
  brms_.stancode           <- utils::getFromNamespace(".stancode", "brms")
  brms_.standata           <- utils::getFromNamespace(".standata", "brms")
  brms_read_brmsfit        <- utils::getFromNamespace("read_brmsfit", "brms")
  brms_as_one_character    <- utils::getFromNamespace("as_one_character", "brms")
  brms_normalize_stancode  <- utils::getFromNamespace("normalize_stancode", "brms")
  brms_is_like_factor      <- utils::getFromNamespace("is_like_factor", "brms")
  brms_is_equal            <- utils::getFromNamespace("is_equal", "brms")

  utils::getFromNamespace('duplicated.brmsprior', 'brms')

  # validate arguments later passed to Stan
  algorithm <- match.arg(algorithm, brms_algorithm_choices())
  backend   <- match.arg(backend, brms_backend_choices())


  normalize <- brms_as_one_logical(normalize)
  silent    <- brms_validate_silent(silent)
  iter      <- brms_as_one_numeric(iter)
  warmup    <- brms_as_one_numeric(warmup)
  thin      <- brms_as_one_numeric(thin)
  chains    <- brms_as_one_numeric(chains)
  cores     <- brms_as_one_numeric(cores)
  init      <- brms_use_alias(init, inits)
  threads   <- brms_validate_threads(threads)
  opencl    <- brms_validate_opencl(opencl)
  future    <- brms_as_one_logical(future) && chains > 0L
  seed      <- brms_as_one_numeric(seed, allow_na = TRUE)
  empty     <- brms_as_one_logical(empty)
  rename    <- brms_as_one_logical(rename)

  # build new model
  formula <- brms_validate_formula(
    formula, data = data, family = family,
    autocor = autocor, sparse = sparse,
    cov_ranef = cov_ranef
  )

  bterms <- brms::brmsterms(formula)
  data2 <- brms_validate_data2(
    data2, bterms = bterms,
    brms_get_data2_autocor(formula),
    brms_get_data2_cov_ranef(formula)
  )
  data <- brms_validate_data(
    data, bterms = bterms,
    data2 = data2, knots = knots,
    drop_unused_levels = drop_unused_levels,
    data_name = brms_substitute_name(data)
  )
  bframe <- brms_brmsframe(bterms, data)
  prior <- brms_.validate_prior(
    prior, bframe = bframe,
    sample_prior = sample_prior
  )
  stanvars <- brms_validate_stanvars(stanvars, stan_funs = stan_funs)

  # generate Stan code
  model <- brms_.stancode(
    bframe, prior = prior, stanvars = stanvars,
    save_model = save_model, backend = backend, threads = threads,
    opencl = opencl, normalize = normalize
  )

  sdata <- brms_.standata(
    bframe, data = data, prior = prior, data2 = data2,
    stanvars = stanvars, threads = threads
  )

  # Load model from file
  if (!is.null(fit) && inherits(fit, "brmsfit")) {
    x_from_file <- fit
  } else {
    x_from_file <- brms_read_brmsfit(file)
  }

  needs_refit <- brmsfit_needs_refit2(
    x_from_file, scode = model, sdata = sdata, data = data,
    algorithm = algorithm, verbose = verbose,
    formula = formula, backend = backend, prior = prior,
    control = control
  )

  needs_refit
}

#' Diagnostics for model refitting
#'
#' Extension of `brms::brmsfit_needs_refit`
#'
#' @details
#'
#' Checks for the following things and returns a list with the names below:
#'
#' - `software_version_changed`: Did the brms, cmdstan, cmdstanr, rstan, or
#'    StanHeaders package versions change compared to the cached model?
#' - `data_dim_changed`: Did the dimensions (`[rows,columns]`) of the data change?
#' - `backend_changed`: Did the backend change (eg from cmdstanr to rstan)?
#' - `formula_changed`: Did the formula change?
#' - `scode_changed`: Did the generated stan code change?
#' - `factors_changed`: Did the levels of any factors change?
#' - `contrasts_changed`: Did the contrasts of any factors change? Variables
#' that are implicitly coerced to factors (such as character values) are assumed
#' to use contr.treatment by default.
#' - `algorithm_changed`: Did the algorithm change?
#' - `priors_changed`: Did any priors change?
#'
#'
#' @param fit Previously fit model
#' @param sdata Generated standata
#' @param scode Generated stancode
#' @param data Validated data
#' @param algorithm algorithm choice
#' @param verbose verbosity,default TRUE
#' @param formula New formula
#' @param backend backend choice
#' @param prior List of priors
#' @param control brm control argument
#'
#' @return List of logical values denoting which diagnostics indicate the model
#' should be refit
brmsfit_needs_refit2 <- function (fit,
                                  sdata = NULL,
                                  scode = NULL,
                                  data = NULL,
                                  algorithm = NULL,
                                  verbose = FALSE,
                                  formula = NULL,
                                  backend = NULL,
                                  prior = NULL,
                                  control = NULL)
{
  if (is.null(fit)) {
    cli::cli_alert_info("No previous fit found, treating as new model to be fit.")
    return(make_refit_diagnostics(TRUE))
  }
  brms_as_one_logical   <- utils::getFromNamespace("as_one_logical", "brms")
  brms_as_one_character <- utils::getFromNamespace("as_one_character", "brms")

  stopifnot(brms::is.brmsfit(fit))
  verbose <- brms_as_one_logical(verbose)

  if (!is.null(scode)) {
    scode <- brms_as_one_character(scode)
    cached_scode <- brms::stancode(fit)
  }
  if (!is.null(sdata)) {
    stopifnot(is.list(sdata))
    cached_sdata <- tryCatch(brms::standata(fit), error = \(e) e)
  }
  if (!is.null(data)) {
    stopifnot(is.data.frame(data))
    cached_data <- fit[["data"]]
  }
  if (!is.null(algorithm)) {
    algorithm <- brms_as_one_character(algorithm)
    stopifnot(!is.null(fit[["algorithm"]]))
  }

  refit_diagnostics <- make_refit_diagnostics(FALSE)
  refit_diagnostics[['data_dim_changed']] <- check_data_dims(fit, data)
  refit_diagnostics[["y_changed"]] <- compare_dv(fit, data, verbose)

  if (isTRUE(all.equal(formula, fit[["formula"]]))) {
    cli::cli_alert_success("Formula matches cached formula")
  } else {
    cli::cli_alert_danger("Formula doesn't match cached formula, see below:")
    compare_brmsfit_formulas(formula, fit[["formula"]])
    refit_diagnostics[['formula_changed']] <- TRUE
  }

  refit_diagnostics[['software_version_changed']] <- check_versions(fit)
  refit_diagnostics[['backend_changed']] <- check_backend(fit, backend)
  refit_diagnostics[["ad_changed"]] <- check_adapt_delta(control, fit)
  refit_diagnostics[['scode_changed']] <- check_stancode(scode,
                                                         cached_scode,
                                                         verbose)
  refit_diagnostics[['sdata_changed']] <- check_standata(sdata,
                                                         cached_sdata,
                                                         verbose)

  # Data check sets two values & needs to lookup if the formula changed
  refit_diagnostics <- check_data(fit, data, cached_data, refit_diagnostics)
  refit_diagnostics
}

#' Create a refit diagnostic list
#'
#' Helper to create a list of diagnostics filled with the given logical
#' value. Mainly used with FALSE, but also used with TRUE to return early when
#' no saved fit is found.
#'
#' @param value logical, either TRUE or FALSE
#'
#' @return named list of logical values.
make_refit_diagnostics <- function(value) {
  refit_diagnostics <- list(
    software_version_changed = value,
    data_dim_changed         = value,
    backend_changed          = value,
    formula_changed          = value,
    y_changed                = value,
    scode_changed            = value,
    sdata_changed            = value,
    factors_changed          = value,
    contrasts_changed        = value,
    algorithm_changed        = value,
    priors_changed           = value,
    ad_changed               = value
  )
}

#' Determine whether model should be refit based on diagnostics
#'
#' Returns "always" if the model should be refit based on diagnostics, or
#' "never" if either the diagnostics are okay.
#'
#' Note that while you can use this to determine whether or not you want to
#' refit a model, updates to packages/software often bring improvements and
#' bug fixes. The intent is to use this interactively when you haven't set up
#' by-project environments for package versions & don't want to spend time
#' refitting expensive models when all that's changed is a minor software
#' update.
#'
#' @param refit_diagnostics List of diagnostics from `diagnose_refit`
#' @param ignore Specific diagnostics to ignore when refitting. Ignores
#' software version changes and stancode differences by default.
#'
#' @return String to pass to `file_refit` parameter of `brm()`
determine_refit <- function(refit_diagnostics,
                            ignore = c("software_version_changed", "scode_changed")) {
  diagnostics <- names(refit_diagnostics)
  if (length(ignore) > 1 || (length(ignore) == 1 && !is.null(ignore))) {
    stopifnot(all(ignore %in% diagnostics))

    refit_diagnostics <- refit_diagnostics[!diagnostics %in% ignore]
  }

  if (any(unlist(refit_diagnostics)))
    return ("always")

  "never"
}

#' Compare response variables
#'
#'
#' @param fit Model fit
#' @param data Dataframe
#' @param verbose Whether to print differences in attributes, if any
#'
#' @return Logical value depending on whether the values of the response
#' variable differ
compare_dv <- function(fit, data, verbose = FALSE) {
  response_variable <- as.character(fit[["formula"]][["formula"]][[2]])
  new_dv <- data[[response_variable]]
  cached_dv <- fit[['data']][[response_variable]]

  has_changed <- FALSE

  values_equivalent <- suppressWarnings(all.equal(cached_dv, new_dv, check.attributes = FALSE))



  if (isTRUE(values_equivalent)) {
    cli::cli_alert_success(paste0("All values for `", response_variable, "` match"))
  } else {
    cli::cli_alert_danger(paste0("Values for `", response_variable, "` do not match"))
    has_changed <- TRUE
  }

  if (verbose) {
    attrs_equivalent <- suppressWarnings(all.equal(attributes(cached_dv),
                                                   attributes(new_dv)))

    if (!isTRUE(attrs_equivalent))
      cli::cli_alert_info(paste0("Attributes for ", response_variable, " don't match:"))
    print(attrs_equivalent)
  }

  has_changed
}

#' Diagnose a brm() call
#'
#' "wrapper function" isn't quite the right term for this, but this function
#' takes a `brm(...)` call, defuses it, checks the refit diagnostics using the
#' arguments in that call, then executes the original call. Usage is
#' `wrap_diagnose(brm(...))`.
#'
#' @details
#' If you have a brm() call already set up and want to programmatically check
#' the refit diagnostics, then refit according to some decision criterion, then
#' you'll need to use some metaprogramming techniques to run both
#' `diagnose_refit` and `brm`. This function will defuse a `brm(...)` call,
#' run `diagnose_refit(...)` with the same passed arguments, then execute the
#' original `brm(...)` call with `file_refit` changed to the output of
#' `determine_refit(diagnose_refit(...))`. The final model is then returned.
#' If the model doesn't need to be refit, the original model is returned.
#' If the model does need to be refit, then it is recompiled and refit.
#' If you no longer need this (ie once the analysis is finished), then you can
#' just remove the `wrap_diagnose` text and be done without having to clean up
#' any metaprogramming. Compare the example here to the example for `diagnose_refit`
#'
#'
#' @param brm_call A `brm(...)` call, where `...` contains all the requisite
#' arguments for fitting a model, including `file`
#' @param skip_refit Logical,d efault `FALSE`, whether to skip refitting the
#' model and return the file saved to the disk.
#' @return Executes the `brm(...)` call, refitting according to `determine_refit`
#'
#' @examples
#' \dontrun{
#'
#' # Example of using this in a function that sets up the hyper parameters already
#' # When we no longer need it, we can just delete the `wrap_diagnose`
#' fit_generic_model <- function(formula, filename, data=model_data, priors=base_model_priors) {
#'   # We need the full brm call, so we keep it in an expression.
#'   mdl <- wrap_diagnose(brm(formula = formula,
#'                                data = data,
#'                                family = lognormal(),
#'                                seed = 111,
#'                                cores = 4,
#'                                chains = 4,
#'                                threads = threading(3),
#'                                file = file.path(here::here("Models"), paste0(filename, ".rds")),
#'                                file_refit = "on_change",
#'                                iter = 10000,
#'                                warmup = 2000,
#'                                thin = 2,
#'                                silent = 0,
#'                                verbose = TRUE,
#'                                save_pars = save_pars(all = TRUE),
#'                                backend = 'cmdstanr',
#'                                prior = priors))
#'
#'   mdl
#' }
#' }
#'
wrap_diagnose <- function(brm_call, skip_refit = FALSE) {
  brms_read_brmsfit <- utils::getFromNamespace("read_brmsfit", "brms")

  brm_quo <- rlang::enquo(brm_call)
  brm_env <- rlang::quo_get_env(brm_quo)
  brm_expr <- rlang::quo_get_expr(brm_quo)

  # Create a call to diagnose_refit
  diagnose_expr <- brm_expr
  diagnose_expr[[1]] <- rlang::sym("diagnose_refit")

  filename <- eval(brm_expr[["file"]], brm_env) # Get the filename from the call

  # Load the file then pass to the diagnose expression,
  # otherwise the file will be loaded twice
  file_annotation <- ""

  if (file.exists(filename)) {
    filesize <- file.size(filename) / 1024^2
    file_unit <- "MB"

    if (filesize > 1024) {
      filesize <- filesize / 1024
      file_unit <- "GB"
    }
    filesize <- round(filesize,2)

    file_annotation <- paste0(" (", filesize, file_unit, ")")
  }



  cli::cli_progress_step(paste0("Loading `", basename(filename), "`", file_annotation),
                         spinner = TRUE)
  fit <- brms_read_brmsfit(filename) # Returns NULL if file not found
  cli::cli_progress_done()
  diagnose_expr[["fit"]] <- fit

  # Evaluate the constructed call in the original call's environment
  should_refit <- determine_refit(eval(diagnose_expr, brm_env))
  message("Refit status for ", basename(filename), ": ", should_refit)


  # Replace the value of file_refit with the diagnostic determination
  brm_expr[["file_refit"]] <- should_refit
  diagnose_expr[["fit"]] <- fit

  if (skip_refit){
    message("Skipping model refit and loading from file...")
    brm_expr[["file_refit"]] <- "never"
  }

  # NOW we can execute the model fitting call
  mdl <- eval(brm_expr, brm_env)

  mdl
}

#' Check brmsfit formulas
#'
#' Goes through each component of the formulas used in brmsfit objects
#' and tries to determine which things are different.
#'
#' @details
#'
#' Will report the following:
#'
#'  - Whether the main part of the formula is different
#'  - Whether the pforms (additional parameters like sigma or disc) are
#'  different. Specifically, whether and which pforms were added, removed, or
#'  modified.
#'  - Whether the response variable changed
#'  - Whether the likelihood family changed
#'  - Whether the mecor changed (I dont know what this is though)
#'
#' @param f1 New formula
#' @param f2 Cached formula from `fit[["formula"]]`
compare_brmsfit_formulas <- function(f1, f2) {
  # Check if the main part of the formula is different

  if (!identical(f1[["formula"]], f2[["formula"]], ignore.environment = TRUE)){
    cli::cli_alert_danger("bf formula mismatch:")
    cat(paste0("New formula:\n"))
    print(f1)

    cat(paste0("Cached formula:\n"))
    print(f2)

  } else {
    cli::cli_alert_info("bf formula matches...")
  }

  # Check if the pforms differ
  if (!identical(f1[["pforms"]], f2[["pforms"]], ignore.environment = TRUE)) {
    cli::cli_alert_danger("Pforms don't match:")
    f1_pforms <- names(f1[["pforms"]])
    f2_pforms <- names(f2[["pforms"]])

    removed_pforms <- f2_pforms[!f2_pforms %in% f1_pforms]
    added_pforms <- f1_pforms[!f1_pforms %in% f2_pforms]
    existing_pforms <- f1_pforms[f1_pforms %in% f2_pforms]

    if (!identical(added_pforms, character(0)) && !is.null(added_pforms)) {
      cli::cli_alert_warning(paste0("Added: ", paste0(added_pforms, collapse = ", ")))
    } else {
      cli::cli_alert_info("No pforms added...")
    }
    if (!identical(removed_pforms, character(0)) && !is.null(removed_pforms)) {
      cli::cli_alert_warning(paste0("Removed: ", paste0(removed_pforms, collapse = ", ")))
    } else {
      cli::cli_alert_info("No pforms removed...")
    }

    which_pforms_changed <-
      vapply(existing_pforms,
             \(pf) !identical(f1[["pforms"]][[pf]],
                              f2[["pforms"]][[pf]],
                              ignore.environment = TRUE),
             logical(1))

    if (!identical(which_pforms_changed, logical(0)) && any(which_pforms_changed)){
      cli::cli_alert_warning(
        paste0("Modified: ", paste0(existing_pforms[which_pforms_changed], collapse = ", "))
      )
    } else {
      cli::cli_alert_info("No pforms modified...")
    }

  } else {
    cli::cli_alert_info("Pforms match...")
  }

  # Check if the response variable changed
  if (!identical(f1[["resp"]], f2[["resp"]], ignore.environment = TRUE)) {
    cli::cli_alert_danger(paste0("Response variable changed: ", f1[["resp"]], " vs cached ", f2[["resp"]]))
  } else {
    cli::cli_alert_info("Response matches...")
  }

  # Check if the likelihood family changed, does not go into detail about
  # each part of the f1[["family"]] elements
  if (!identical(f1[["family"]], f2[["family"]], ignore.environment = TRUE)) {
    cli::cli_alert_danger("Family changed:")
    cat("New formula:\n")
    print(f1[["family"]])

    cat("Cached formula:\n")
    print(f1[["family"]])
  } else {
    cli::cli_alert_info("Family matches...")
  }

  # Check if the mecor changed
  if (!identical(f1[["mecor"]], f2[["mecor"]], ignore.environment = TRUE)) {
    cli::cli_alert_danger("Mecor mismatches.")
  } else {
    cli::cli_alert_info("Mecor matches...")
  }
}

check_kfold_refit <- function(mdl, kfold_fit) {
  mdl_stancode <- brms::stancode(mdl)
  kfold_stancode <- brms::stancode(kfold_fit[["fits"]][1,][["fit"]])

  mdl_dims <- dim(mdl[["data"]])
  kfold_dims <- dim(kfold_fit[["data"]])

  stancode_equivalent <- identical(mdl_stancode, kfold_stancode)
  data_dims_equivalent <- identical(mdl_dims, kfold_dims)

  formula_equivalent <- isTRUE(all.equal(kfold_fit[["fits"]][1,][["fit"]][["formula"]],
                                         mdl[["formula"]]))

  should_refit <- FALSE
  if (stancode_equivalent) {
    cli::cli_alert_success("Stancode equivalent")
  } else {
    cli::cli_alert_danger("Stancode mismatches")
    should_refit <- TRUE
  }

  mdl_dims <- paste0("[", mdl_dims, "]", collapse = ",")

  if (data_dims_equivalent) {
    cli::cli_alert_success(glue::glue("Data dims match\t{mdl_dims}"))
  } else {
    kfold_dims <- paste0("[", kfold_dims, "]", collapse = ",")
    cli::cli_alert_danger(glue::glue("Data dims mismatch:\t{mdl_dims} {kfold_dims}"))
    should_refit <- TRUE
  }

  kfold_control <- kfold_fit[["fits"]][,1][[1]][["stan_args"]]
  should_refit <- check_adapt_delta(kfold_control, mdl) | should_refit

  if (formula_equivalent) {
    cli::cli_alert_success(glue::glue("Formulas match"))
  } else {
    cli::cli_alert_danger(glue::glue("Formulas mismatch"))
    should_refit <- TRUE
  }

  should_refit
}

check_adapt_delta <- function(control, mdl) {
  ad_changed <- FALSE

  # brmsfit object should always have this, but would be NULL if it didnt anyways
  # control_identical <- identical(mdl[["stan_args"]][["control"]], control)


  # If this isn't explicitly set when fitting the model, it will be NULL,
  # but will be filled in later with the default of 0.8. We need to make sure
  # that an implicit 0.8 and an explicit 0.8 aren't treated as different
  ad_new <- control[["adapt_delta"]] %||% 0.8
  ad_old <- mdl[["stan_args"]][["control"]][["adapt_delta"]] %||% 0.8

  if (ad_old != ad_new) {
    cli::cli_alert_warning(glue::glue("Adapt delta changed: {ad_old}->{ad_new}"))
    ad_changed <- TRUE
  } else {
    cli::cli_alert_success("Adapt delta matches")
  }


  ad_changed
}


#' Wrapper to run kfold cross validation with preset fold assignments
#'
#' Given a `brmsfit` object, run kfold cross validation on that model. Each
#' row in the dataframe should be associated with an integer assignment provided
#' by `fold_assignments`.
#'
#' @details
#' `k` should be specified to the desired number of folds and should equal the
#' max number in `fold_assignments`. The model will be refit `k` times, which
#' can take a very long time. This function will use `future` to parallelize
#' the models. `nworkers` should be set to the same value as `k` for maximum
#' efficiency, but can be set to a lower value if there are many folds and chains.
#'
#' For example, to fit 5 fold models at once, you can have `nworkers=5` and
#' `cores_per_fold=1`, which will fit each *model* in parallel but each *chain*
#' of those models sequentially. You can raise `cores_per_fold` to 2 to fit
#' two chains for each model in parallel. But note that this raises the number
#' of required *physical* cores from 5 to 10.
#'
#' Because refitting the model is so expensive, this function will perform a
#' number of diagnostic tests to verify that the provided `mdl` has changed
#' compared to the last time kfold CV was run.
#'
#' @param mdl `brmsfit` object
#' @param kfold_path Path to save the kfold-cv model to. Note that this is
#' going to be a list of brmsfit objects with some overhead functionality on top.
#' @param fold_assignments Integer vector of length `nrow(mdl$data)` assigning
#' each row/observation to a fold. This is done outside of this function to
#' ensure that the folds are consistent and to allow flexibility in however the
#' user decides to stratify the folds. Max value must equal `k`
#' @param k Number of folds
#' @param nworkers Number of workers to use for parallelization
#' @param cores_per_fold Number of cores to use for each model fitting job
#' @param nchains Number of chains to fit for each model. If `k` is very large,
#' you might want to lower this from 4 to 2.
#'
#' @returns `kfold_path`, the PATH to a saved kfold object
#' @export
fit_kfold <- function(mdl,
                      kfold_path,
                      fold_assignments,
                      k = 5,
                      nworkers = 5,
                      cores_per_fold = 1,
                      nchains = 4) {
  rlang::is_installed("future")

  # Ensure that the fold assignments match the length of the data used to
  # fit the original model
  mdl_name <- deparse1(substitute(mdl))
  stopifnot(length(fold_assignments) == nrow(mdl[["data"]]))

  stopifnot(max(fold_assignments) != k)

  if (cores_per_fold * nworkers > parallel::detectCores(logical = FALSE)) {
    warning("Number of requested cores is more than the number of available phsyical cores.")
  }

  # Load the previous kfold fit if it exists
  cli::cli_progress_step(paste0("Loading `", basename(kfold_path), "`"),
                         spinner = TRUE)
  kfold_fit <- tryCatch(readRDS(kfold_path), error = \(e) NULL)
  cli::cli_progress_done()

  if (is.null(kfold_fit)) {
    cli::cli_alert_info("No previous fit found, treating as new model to be fit.")
    should_refit <- TRUE
  } else {
    should_refit <- check_kfold_refit(mdl, kfold_fit)
  }

  if (should_refit) {
    cli::cli_alert_warning(paste0("Refitting kfold at ", Sys.time()))

    rm(kfold_fit) # Drop from memory before creating child processes
    gc()

    future::plan(future::multisession, workers = nworkers)
    kfold_fit <- brms::kfold(mdl,
                             K = k,
                             folds = fold_assignments,
                             compare = TRUE,
                             cores = cores_per_fold,
                             chains = nchains,
                             save_fits = TRUE)
    future::plan(future::sequential)

    cli::cli_alert_info(paste0("Saving kfold at ", Sys.time()))
    attr(kfold_fit, "model_name") <- mdl_name
    saveRDS(kfold_fit, kfold_path)
  }

  # Slightly redundant, but if the kfold fit is older & the model name isn't
  # set right then we need to fix that even though we didn't refit anything
  if (attr(kfold_fit, "model_name") == "mdl") {
    attr(kfold_fit, "model_name") <- mdl_name
    saveRDS(kfold_fit, kfold_path)
  }

  kfold_path
}

check_data_dims <- function(fit, data) {
  should_refit <- FALSE

  # Check data dimensions
  cached_data_dims <- dim(fit[['data']])
  data_dims <- dim(data)
  same_dims <- identical(cached_data_dims, data_dims)
  data_dims <- paste0("[",data_dims[1],",",data_dims[2],"]")

  if(same_dims){
    cli::cli_alert_success(glue::glue("Data dims unchanged:\t{data_dims}"))
  } else {
    cached_data_dims <- paste0("[",cached_data_dims[1],",",cached_data_dims[2],"]")
    cli::cli_alert_danger(
      glue::glue("Data dims changed:\t{data_dims} vs cached {cached_data_dims}")
    )
    should_refit <- TRUE
  }

  should_refit
}

check_versions <- function(fit) {
  should_refit <- FALSE

  software_versions <- list(
    brms       = tryCatch(as.character(utils::packageVersion("brms")),        error = \(e) "NULL"),
    cmdstanr   = tryCatch(as.character(utils::packageVersion("cmdstanr")),    error = \(e) "NULL"),
    rstan      = tryCatch(as.character(utils::packageVersion("rstan")),       error = \(e) "NULL"),
    stanheader = tryCatch(as.character(utils::packageVersion("StanHeaders")), error = \(e) "NULL"),
    cmdstan    = tryCatch(as.character(cmdstanr::cmdstan_version()),   error = \(e) "NULL")
  )

  cached_versions <- list(
    brms     =   as.character(fit[['version']][['brms']]        %||% "NULL"),
    cmdstanr =   as.character(fit[['version']][['cmdstanr']]    %||% "NULL"),
    rstan    =   as.character(fit[['version']][['rstan']]       %||% "NULL"),
    stanheader = as.character(fit[['version']][['stanHeaders']] %||% "NULL"),
    cmdstan    = as.character(fit[['version']][['cmdstan']]     %||% "NULL")
  )


  for (soft_name in names(software_versions)) {
    same_version <- identical(software_versions[[soft_name]],
                              cached_versions[[soft_name]])

    if (same_version){
      cli::cli_alert_success(
        glue::glue("[{soft_name}]\tOK        ({software_versions[[soft_name]]})")
      )
    } else {
      cli::cli_alert_warning(
        glue::glue("[{soft_name}]\tMismatch  ({software_versions[[soft_name]]})\tvs cached ({cached_versions[[soft_name]]})")
      )
      should_refit <- TRUE
    }
  }

  should_refit

}

check_backend <- function(fit, backend) {
  should_refit <- FALSE

  cached_backend <- fit[['backend']]

  if (identical(backend, cached_backend)) {
    cli::cli_alert_success(paste0("Backend match: ", cached_backend))
  } else {
    cli::cli_alert_danger(
      paste0("Backend mismatch: ", backend, " vs cached ", cached_backend)
    )
    should_refit <- TRUE
  }

  should_refit
}

check_stancode <- function(scode, cached_scode, verbose = FALSE) {
  rlang::is_installed("diffobj")
  should_refit <- FALSE
  brms_normalize_stancode <- utils::getFromNamespace("normalize_stancode", "brms")

  if (!is.null(scode)) {
    if (brms_normalize_stancode(scode) !=
        brms_normalize_stancode(cached_scode)) {
      cli::cli_alert_warning("Stan code has changed beyond whitespace/comments.")
      if (verbose) {
        print(diffobj::diffChr(scode, cached_scode, format = "ansi8"))
      }
      should_refit <- TRUE
    }
  }

  should_refit
}

check_standata <- function(sdata, cached_sdata, verbose = FALSE) {
  should_refit <- FALSE

  if (!is.null(sdata)) {
    sdata_equality <- all.equal(sdata, cached_sdata, check.attributes = FALSE)
    if (!isTRUE(sdata_equality)) {
      cli::cli_alert_danger("The processed data for Stan has changed.")
      if (inherits(cached_sdata, "error"))
        cli::cli_alert_info("Error retrieving cached standata, brms versions likely differ greatly.")
      if (verbose) {
        print(sdata_equality)
      }
      should_refit <- TRUE
    }
  }

  should_refit
}

#' @importFrom stats contrasts
check_data <- function(fit, data, cached_data, refit_diagnostics) {
  brms_is_like_factor <- utils::getFromNamespace("is_like_factor", "brms")
  brms_is_equal <- utils::getFromNamespace("is_equal", "brms")

  if (!is.null(data)) {
    factor_level_message <- FALSE

    cached_colnames <- names(cached_data)[-1]

    which_are_factors <-
      vapply(cached_colnames,
             \(var) brms_is_like_factor(cached_data[[var]]) && var != fit[["formula"]][["resp"]],
             logical(1))

    which_factors_changed_levels <-
      vapply(cached_colnames[which_are_factors],
             \(var) {
               # Cast to factor to make sure the actual levels in the data
               # are the same & we're not pulling from a hard-coded superset
               cached_levels <- levels(factor(cached_data[[var]]))
               new_levels <- levels(factor(data[[var]]))

               # Check if levels have changed
               !brms_is_equal(cached_levels, new_levels)
             },
             logical(1),USE.NAMES = TRUE)

    which_factors_changed_contrasts <-
      vapply(cached_colnames[which_are_factors],
             \(var) {
               # Logical and character values are coerced to factors and use
               # the default contrasts, so we'll assume those will always use
               # treatment coding with alphabetical ordering
               if (!is.factor(cached_data[[var]]) | !is.factor(data[[var]]))
                 return(FALSE)

               cached_contrasts <- contrasts(cached_data[[var]])
               new_contrasts <- contrasts(data[[var]])

               # Check if levels have changed
               !identical(cached_contrasts, new_contrasts)
             },
             logical(1),USE.NAMES = TRUE)

    if (any(which_factors_changed_levels)) {
      refit_diagnostics[["factors_changed"]] <- TRUE
      if (refit_diagnostics[["formula_changed"]]) {
        cli::cli_alert_info("Factor checks may be misleading due to change in formula")
      }
      cli::cli_alert_danger(
        paste0("Factor levels don't match for vars: ",
               paste0(cached_colnames[which_are_factors][which_factors_changed_levels],
                      collapse = ", ")))

    } else {
      cli::cli_alert_success("All factor levels match.")
    }

    if (any(which_factors_changed_contrasts)) {
      refit_diagnostics[["contrasts_changed"]] <- TRUE
      cli::cli_alert_danger(
        paste0("Contrasts don't match for vars: ",
               paste0(cached_colnames[which_are_factors][which_factors_changed_contrasts],
                      collapse = ", ")))
    } else {
      cli::cli_alert_success("All factor contrasts match.")
    }

  }

  refit_diagnostics
}


check_kfold_data <- function(kfold_fit, data) {
  kfold_data <- kfold_fit[["data"]]
  cols_to_check <- colnames(kfold_data)
  data <- dplyr::select(data, dplyr::all_of(colnames(kfold_data)))

  should_refit <- FALSE

  col_matches <-
    vapply(cols_to_check,
           \(df_col) {
             identical(kfold_data[[df_col]],
                       data[[df_col]])
           }, logical(1), USE.NAMES  = TRUE)

  if (!all(col_matches)) {
    which_cols <- paste0(names(col_matches)[!col_matches], collapse = ", ")
    cli::cli_alert_danger(glue::glue("Values have changed for: {which_cols}"))
    should_refit <- TRUE
  } else {
    cli::cli_alert_success(glue::glue("All columns match"))
  }

  should_refit
}
