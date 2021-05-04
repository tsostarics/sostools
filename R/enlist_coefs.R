#' Enlist Coefficients
#'
#' Takes a model object and creates a list of coefficients that can be referenced
#' throughout an R markdown file.
#'
#' @param mdl A model object with a summary method
#'
#' @return A list of coefficient descriptions of the form (Beta = b, s.e.=se, z=z, p=p)
#' for latex output
#' @export
#'
#' @examples
#' enlist_coefs(lm(mpg~cyl, data = mtcars))
enlist_coefs <- function(mdl){
  coefs <-
    summary(mdl)$coefficients %>%
    tibble::as_tibble(rownames = 'coefficient',
                      .name_repair = function(x) gsub("[ .()|>]","",x)) %>%
    dplyr::mutate(dplyr::across(Estimate:tidyselect::last_col(offset = 1L), # change this to be more general later
                                function(x)
                                  round(x,2)
    )
    ) %>%
    dplyr::group_by(coefficient) %>%
    dplyr::group_split() %>%
    setNames(.,
             gsub("[ .()|>]", "", vapply(.,
                                         function(x)
                                           x[["coefficient"]], "char"
             )
             )
    )

  purrr::lmap(coefs, function(x) setNames(list(format_coef(names(x), coefs)), names(x)))
}
