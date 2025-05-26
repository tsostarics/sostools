#' Repeat geom changing a small set of values
#'
#' Helper to repeat a geom multiple times swapping in different columns or
#' constant values. Useful for things like drawing a white underdrawing beneath
#' a layer.
#'
#'
#' @param geom_call A geom layer with an aesthetics mapping.
#' @param ... A named series of values to use for aesthetic mappings.
#'
#' @return list of geoms
#' @export
#'
#' @examples
#' \dontrun{
#'
#' rep_geom(geom_line(aes(x = x, y = y),
#'          linewidth = c(3, 1),
#'          color = c("white", color_group)))
#'
#' # == is the same as ==>
#'
#' list(
#'   geom_line(aes(x = x, y = y), linewidth = 3, color = "white"),
#'   geom_line(aes(x = x , y = y, color = color_grp), linewidth = 3)
#' )
#'
#'
#' # color is recycled to the length of the other aesthetics
#' ggplot(mtcars, aes(x = drat, y = mpg)) +
#'   rep_geom(geom_point(shape = 21),
#'            size = c(8, 6, 2),
#'            fill = c("gray80", am, "red"),
#'            color = "yellow")
#'
#' # use !! when passing a variable containing a constant value
#' foo <- "green"
#' ggplot(mtcars, aes(x = drat, y = mpg)) +
#'   rep_geom(geom_point(shape = 21),
#'            size = c(8, 6, 2),
#'            fill = c(!!foo, am, "red"), # <- just passing foo won't work
#'            color = "yellow")
#'            }
rep_geom <- function(geom_call, ...) {
  requireNamespace("ggplot2", quietly = TRUE)
  # Extract quosure from the call so that any passed symbols aren't evaluated
  q <- rlang::enquos(...)

  # There's certainly a better way to do this but this is fine
  aes_to_set <- gsub("color", "colour", names(q))
  names(q) <- aes_to_set

  aes_values <- lapply(aes_to_set,
                       \(x) {
                         aes_expr <- rlang::quo_get_expr(q[[x]])

                         # Remove c() from call
                         if (length(aes_expr) > 1)
                           aes_expr <- aes_expr[-1L]

                         aes_expr
                       })
  names(aes_values) <- aes_to_set

  # Check the lengths of each aesthetic mapping
  lengths <- vapply(aes_values, length, 1L)
  which_to_recycle <- lengths == 1L

  # Throw an error if the non-singleton values aren't the same length
  max_length <- max(lengths)
  if (!all(lengths[!which_to_recycle] == max_length))
    stop("Length of values for each aesthetic must be the same or length 1 (recycled)")

  # Recycle the aesthetic mappings of length 1 to length max_length
  if (length(which_to_recycle) > 0L) {
    for (aesthetic in aes_to_set[which_to_recycle]) {
      aes_values[[aesthetic]][seq_len(max_length)] <- aes_values[[aesthetic]]
    }
  }

  # Make copies of the provided geom and replace the aesthetics by-index
  lapply(seq_len(max_length),
         \(i) {

           this_geom <- ggplot2::ggproto(NULL, geom_call) # Essentially deep copies the geom

           for (which_aes in aes_to_set) {
             aes_value <- aes_values[[which_aes]][[i]]
             if (inherits(aes_value, "name"))
               this_geom[["mapping"]][[which_aes]] <- rlang::set_expr(this_geom[["mapping"]][[which_aes]], aes_value)
             else
               this_geom[["aes_params"]][[which_aes]] <- aes_value
           }

           this_geom
         })
}
