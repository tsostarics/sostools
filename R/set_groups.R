#' Establish plotting groups based on regions of significant differences
#'
#' Given a logical vector, determine groups of contiguous TRUE or FALSE values
#'
#' @param logicals Logical vector
#'
#' @returns Integer indices mapping each value in `logicals` to a group
set_groups <- function(logicals) {
  groups <- logicals[NA]

  prev_val <- logicals[1]
  g <- 1L
  groups[1] <- g
  for (i in seq_along(logicals)[-1]) {
    cur_val <- logicals[i]
    if (cur_val != prev_val)
      g <- g + 1L
    prev_val <- cur_val
    groups[i] <- g
  }

  groups
}
