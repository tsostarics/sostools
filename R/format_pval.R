#' Title
#' Helper to format p values in inline prose;
#' src: https://www.eyer.us/post/2019-10-20-f_pvalue/
#'
#' @param p.value double
#' @param symbol set to =
#' @param use_stars Should add significance stars (.05, .01, .001) default FALSE
#'
#' @return a string for a p value (no stars!)
#' @export
format_pval = function(p.value, symbol = "=", use_stars = FALSE){
  stars <- ""
  if (use_stars)
    if (p.value < .001) {
      stars <- "***"
    } else if (p.value < .01) {
      stars <- "**"
    } else if (p.value < .05) {
      stars <- "*"
    } else {
      stars <- ""
    }
  p.value <- round(p.value, digits = 3)
  if (p.value == 0) {
    return("p < .001")
  } else {
    return(paste0("p", symbol, p.value, stars))
  }
}
