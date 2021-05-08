#' Enlist Coefficients
#'
#' Takes a model object and creates a list of coefficients that can be referenced
#' throughout an R markdown file.
#'
#' @param mdl A model object with a summary method
#' @param correct Regular expressions targeting coefficients to bonferroni correct
#' @return A list of coefficient descriptions of the form (Beta = b, s.e.=se, z=z, p=p)
#' for latex output
#' @export
#'
#' @examples
#' enlist_coefs(lm(mpg~cyl, data = mtcars))
enlist_coefs <- function(mdl, correct = NA){

  coefs <-
    mdl %>%
    broom::tidy() %>%
    dplyr::mutate(dplyr::across(estimate:statistic, function(x) round(x,2)))

  coefs['coef.type'] <- NULL

  if (any(!is.na(correct)))
    coefs <- .adjust_pvals(coefs, correct)

  statistic_name <- colnames(summary(mdl)$coefficients)[[3L]]

  # Split into list
  coefs <-
    coefs %>%
    dplyr::group_by(term) %>%
    dplyr::group_split() %>%
    setNames(.,
             gsub("[ .()>]", "", vapply(.,
                                        function(x)
                                          x[["term"]], "char"
             )
             )
    )

  purrr::lmap(coefs,
              function(x)
                setNames(list(format_coef(names(x), coefs, statistic_name)),
                         names(x)
                         )
              )
}

.set_cgrp <- function(correct_groups, terms){
  vapply(terms,
         function(x){
           grp <- correct_groups[vapply(correct_groups, function(y) grepl(y,x), TRUE)]
           ifelse(identical(grp, character(0)), "", grp)
         },
         "char",
         USE.NAMES = FALSE)
}

.adjust_pvals <- function(coefs, correct_groups) {
  dplyr::mutate(coefs,
                rownum = dplyr::row_number(),
                cgrp = .set_cgrp(correct_groups, term)) %>%
    dplyr::group_by(cgrp) %>%
    dplyr::group_split() %>%
    lapply(
      function(xdf){
        if (xdf[["cgrp"]][[1L]] == "") return(xdf)
        dplyr::mutate(xdf,
               p.value = p.adjust(p.value))
      }
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(rownum) %>%
    dplyr::select(-rownum, -cgrp)
}
