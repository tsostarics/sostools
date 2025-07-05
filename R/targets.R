# These are helper functions mostly used in interactive sessions.

#' Load model file to current environment
#'
#' Given a model name, load the model to the current environment.
#' eg: tar_load_model(exp1_pabt_WKSLP_mdl)
#'
#' @param model_target_name Name of the target containing a path to an rds file
#'
#' @returns nothing, assigns to current environment
#' @export
tar_load_model <- function(model_target_name) {
  rlang::is_installed("targets")
  target_name <- deparse1(substitute(model_target_name))

  assign(target_name, readRDS((file.path("Models", paste0(target_name, ".rds")))), pos = parent.frame())
}


#' Read model
#'
#' Given a model name (=target name), return the model object associated with it.
#' Note that the stored value of the TARGET is the PATH to the model, so it
#' needs to be read with readRDS. This just wraps around that 2-step process.
#'
#' @param model_target_name Name of the target containing a path to an rds file
#'
#' @returns object saved at the rds file
#' @export
tar_read_model <- function(model_target_name) {
  rlang::is_installed("targets")
  target_name <- deparse1(substitute(model_target_name))

  readRDS(targets::tar_read_raw(target_name))
}



#' Load target from error message
#'
#' In interactive sessions, if a value associated with a target's object is
#' not found (thus throwing an error), tar_load_err() will read the last
#' error message and load the missing object
#'
#' @returns nothing, loads object to current environment
#' @export
tar_load_err <- function() {
  err <- geterrmessage()
  rlang::is_installed("targets")
  object_name <- stringr::str_match(err, "object '([^']+)'")[,2]

  cli::cli_alert_info(paste0("Loading `", object_name, "`"))
  targets::tar_load_raw(object_name,envir = parent.frame(2))
}

#' Load model from error message
#'
#' Same as tar_load_err() but with models specifically
#'
#' @returns nothing, loads model to current environment
#' @export
tar_load_em <- function() {
  err <- geterrmessage()
  rlang::is_installed("targets")
  object_name <- stringr::str_match(err, "object '([^']+)'")[,2]

  cli::cli_alert_info(paste0("Loading `", object_name, "`"))

  assign(object_name, readRDS(targets::tar_read_raw(object_name)), pos = parent.frame())

}
