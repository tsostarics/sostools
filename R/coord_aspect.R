#' Fixed aspect ratio relative to X/Y limits
#'
#' Uses a given aspect ratio for a ggplot based on the limits provided.
#'
#' If the range of X values is 10 to 200 and the range of Y values is 20 to 60,
#' then a square aspect ratio would require a ratio of (200 - 10) / (60 - 40).
#' A landscape 16:9 aspect ratio would be (9/16) * (200-10) / (60-40). This is
#' an annoying amount of things to type out if you're fiddling around with the
#' limits and aspect ratio.
#'
#' @param ratio Numeric ratio, default 1 (for square). Ratios should be of the
#' form height / width.
#' @param xlim Numeric vector of length 2, limits for x axis
#' @param ylim Numeric vector of length 2, limits for y axis
#' @param expand Logical, default FALSE, whether to expand
#' @param clip Whether to clip to plot, default "on"
#'
#' @returns coordinate ggproto
#' @export
coord_aspect <- function(ratio = 1,
                         xlim = NULL,
                         ylim = NULL,
                         expand = FALSE,
                         clip = "on") {
  .check_coord_limits(xlim)
  .check_coord_limits(ylim)
  ggplot2::ggproto(NULL, ggplot2::CoordFixed, limits = list(x = xlim, y = ylim),
          ratio = ratio * (diff(xlim) / diff(ylim)), expand = expand, clip = clip)
}

# Modified from ggplot2's
.check_coord_limits <- function (limits, arg = rlang::caller_arg(limits), call = rlang::caller_env())
{
  if (is.null(limits)) {
    stop("Must provide limits")
  }
  if (!vctrs::obj_is_vector(limits) || length(limits) != 2) {
    what <- "{.obj_type_friendly {limits}}"
    if (is.vector(limits)) {
      what <- paste0(what, " of length {length(limits)}")
    }
    cli::cli_abort(paste0("{.arg {arg}} must be a vector of length 2, not ",
                          what, "."), call = call)
  }
}
