#' Enlist Coefficients
#'
#' Takes a model object and creates a list of coefficients that can be referenced
#' throughout an R markdown file.
#'
#' @param model A model object with a summary method
#' @param correct Regular expressions targeting coefficients to bonferroni correct
#' @param ... Additional arguments if model is a brmsfit, see docs for tidy_brmsfit for options
#' @return A list of coefficient descriptions of the form (Beta = b, s.e.=se, z=z, p=p)
#' for latex output
#' @export
#'
#' @examples
#' enlist_coefs(lm(mpg~cyl, data = mtcars))
enlist_coefs <- function(model, correct = NA, ...){
  if (class(model) == 'brmsfit')
    return(.enlist_brmsfit(model))
  tidy_fx <- .get_tidy_fx(model)

  coefs <-
    model |>
    tidy_fx() |>
    dplyr::mutate(dplyr::across(estimate:statistic, function(x) round(x,2)))

  # Remove coef type column from clmm models
  coefs['coef.type'] <- NULL

  # Filter out random effects if needed
  if ('group' %in% names(coefs))
    coefs <- dplyr::filter(coefs, is.na(group)) |> dplyr::select(-group)

  if (any(!is.na(correct)))
    coefs <- .adjust_pvals(coefs, correct)

  statistic_name <- colnames(summary(model)$coefficients)[[3L]]

  # Split into list
  coefs <- coefs |> split(~term)
  names(coefs) <- vapply(names(coefs), \(x) gsub("[ .()>]", "", x), "char")

  purrr::lmap(coefs,
              \(x)
              setNames(list(format_coef(names(x), coefs, statistic_name)), names(x))
              )
}

.enlist_brmsfit <- function(model, ...) {
  coefs <- tidy_brmsfit(model, include_prior = FALSE, ...) |> split(~term)
  names(coefs) <- vapply(names(coefs), \(x) gsub("[ .()>]", "", x), "char")
  purrr::lmap(coefs,
              \(x) setNames(list(.format_brms_coef(names(x), coefs)), names(x))
              )

}

# Set which groups need to be corrected for multiple comparisons
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
                cgrp = .set_cgrp(correct_groups, term)) |>
    split(~cgrp) |>
    lapply(
      function(xdf){
        if (xdf[["cgrp"]][[1L]] == "") return(xdf)
        dplyr::mutate(xdf,
               p.value = p.adjust(p.value))
      }
    ) |>
    dplyr::bind_rows() |>
    dplyr::arrange(rownum) |>
    dplyr::select(-rownum, -cgrp)
}
