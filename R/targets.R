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



#' Get reverse dependencies of a node
#'
#' Given the name of a target and the edges dataframe from `targets::tar_network()`
#' return the downstream nodes that depend on the given target. Will recurse
#' through the entire DAG.
#'
#' @param from Node to search from
#' @param edges Edges dataframe from tar_network
#'
#' @return Character vector of target names
get_reverse_dependencies <- function(from = "write_constant_scale_files", edges) {
  # Base case
  if (!from %in% edges$from)
    return(from)

  new_nodes <- edges[edges$from == from,][['to']]

  nodes <- lapply(new_nodes, \(x) c(from, get_reverse_dependencies(x, edges)))

  unique(c(nodes, recursive=TRUE))

}

#' Delete and invalidate reverse dependencies
#'
#' Targets doesn't play perfectly with praat scripts since the usual workflow
#' is to create large numbers of files. When these files are recreated, targets
#' doesn't flag them as being out of date since the files themselves aren't
#' tracked (too many to do so and also the repository option wouldn't work for
#' me). To circumvent this, this function will invalidate and delete all of the
#' downstream target information from a given node, which will force them
#' to be rerun.
#'
#' tar_network() can take a while to run, so it's recommended to save this
#' to a variable if you think you might need to rerun this a few times as
#' you try new things in praat.
#'
#' @param from Node to delete from
#' @param network Output of targets::tar_network()
#'
#' @return Character vector of nodes that were invalidated and deleted
flush_targets <- function(from, network = targets::tar_network()) {
  rlang::is_installed("targets")

  nodes_to_flush <- get_reverse_dependencies(from, network$edges)

  targets::tar_invalidate(all_of(nodes_to_flush))
  targets::tar_delete(all_of(nodes_to_flush))

  message(glue::glue("Invalidated and deleted {length(nodes_to_flush)} nodes from {from}"))

  nodes_to_flush
}
