#' Glimpse contrasts in dataframe
#'
#' Uses the same syntax as `enlist_contrasts` and `set_contrasts`. Returns
#' a summary table of the contrasts you've set. If you set `return.list` to TRUE
#' then you can access a list of contrasts in the second element of the resulting
#' list. The glimpse dataframe is the first element. FALSE will return just
#' the glimpse data frame.
#'
#' @param model_data Data to be passed to a model fitting function
#' @param ... Series of formulas
#' @param return.list Logical, defaults to FALSE, whether the output of enlist_contrasts should be
#' returned
#' @param verbose Logical, defaults to FALSE, whether messages should be printed
#' @param all.factors Logical, defaults to TRUE, whether the factors not
#' explicitly set with formulas should be included
#' @param clean.schemes Logical, defaults to FALSE, whether the contrast schemes
#' should remove "contr." and "_code" from the names ("sum" and not "contr.sum"
#' or "sum_code")
#'
#' @return A dataframe is return.list is FALSE, a list with a dataframe and list
#' of named contrasts if TRUE.
#' @export
glimpse_contrasts <- function(model_data,
                              ...,
                              return.list = FALSE,
                              verbose=TRUE,
                              all.factors=TRUE,
                              clean.schemes = FALSE) {
  formulas <- suppressWarnings(rlang::dots_splice(...)) # outer names warning?
  contrast_list <- enlist_contrasts(model_data, ..., 'verbose' = verbose)
  params <- lapply(formulas, .make_parameters)

  # Extract various information
  factor_names <- names(contrast_list)
  factor_sizes <- vapply(contrast_list, nrow, 1L, USE.NAMES = FALSE)
  level_names <- unname(lapply(contrast_list, rownames))
  scheme_labels <- .get_scheme_labels(params, formulas)
  reference_levels <- .get_reference_levels(contrast_list, params, formulas)
  orthogonal_contrasts <- vapply(contrast_list, is_orthogonal, TRUE, USE.NAMES = FALSE)
  intercept_interpretations <- vapply(contrast_list, interpret_intercept,"char", USE.NAMES = FALSE)

  # Double check that dropped trends are only included for polynomial contrasts
  dropped_trends <-  .get_dropped_trends(params, formulas)
  which_are_polynomials <- vapply(scheme_labels, .is_polynomial_scheme, TRUE)
  dropped_trends[!which_are_polynomials] <- NA

  glimpse <- tibble::tibble("factor" = factor_names,
                            "n_levels" = factor_sizes,
                            "level_names" = level_names,
                            "scheme" = scheme_labels,
                            "reference" = reference_levels,
                            "intercept" = intercept_interpretations,
                            "orthogonal" = orthogonal_contrasts,
                            "dropped_trends" = dropped_trends,
                            "explicitly_set" = TRUE)



  if (all.factors)
    glimpse <- rbind(glimpse, .glimpse_default_factors(model_data, factor_names))

  if (clean.schemes)
    glimpse$scheme <- .clean_schemes(glimpse$scheme)

  # The default factors don't need to be specified in the contrast list,
  # they'll just use their respective defaults by the model fitting function
  if (return.list)
    return(list("glimpse" = glimpse, "contrasts" = contrast_list))


  glimpse
}

.clean_schemes <- function(scheme_labels) {
  vapply(scheme_labels,
         function(x) {
           x <- gsub("contr\\.poly","orth_polynomial",x)
           gsub("(^contr\\.)|(_code$)","",x)
         },
         "char",
         USE.NAMES = FALSE
  )
}

#' Glimpse default factors
#'
#' Given a dataframe with some factor columns and a character vector of
#' which columns you've already set yourself, look at all the other
#' factor columns and get a glimpse at how they're treated by the defaults
#' specified in `options('contrasts')`. Reference level is assumed to be the
#' first level for unordered factors and nonexistent for ordered factors.
#'
#' @param model_data Dataframe
#' @param set_factors Explicitly set columns to ignore
#'
#' @return A table with information about the contrasts for all remaining factor
#' columns
.glimpse_default_factors <- function(model_data, set_factors = NULL) {
  factor_cols <- .cols_where(model_data, is.factor, return.names = TRUE)
  new_factors <- factor_cols[!factor_cols %in% set_factors]
  ordered_factors <- .cols_where(model_data, is.ordered, return.names = TRUE)
  is_ordered_factor <- new_factors %in% ordered_factors
  names(is_ordered_factor) <- new_factors

  # Extract contrasts from the factor columns
  new_contrasts <- lapply(new_factors,
                          function(x) contrasts(model_data[[x]]))


  factor_sizes <- vapply(new_factors,
                         function(x) nlevels(model_data[[x]]),
                         FUN.VALUE = 1L)
  level_names <- lapply(new_factors, function(x) levels(model_data[[x]]))
  scheme_labels <- vapply(new_factors,
                          function(x)
                            ifelse(is_ordered_factor[x],
                                   options('contrasts')[[1]]["ordered"],
                                   options('contrasts')[[1]]["unordered"]),
                          FUN.VALUE = "char")
  reference_levels <- vapply(new_factors,
                             function(x)
                               ifelse(is_ordered_factor[x],
                                      NA_character_,
                                      as.character(levels(model_data[[x]])[[1L]])),
                             "char")
  intercept_interpretations <- vapply(new_contrasts, interpret_intercept, "char", USE.NAMES = FALSE)
  orthogonal_contrasts <- vapply(new_contrasts, is_orthogonal, TRUE)
  dropped_trends <- rep(NA, length(new_factors)) # Trends are never dropped w/ R's defaults

  .warn_if_nondefault(new_contrasts, new_factors, factor_sizes, is_ordered_factor)

  glimpse <- tibble::tibble("factor" = new_factors,
                            "n_levels" = factor_sizes,
                            "level_names" = level_names,
                            "scheme" = scheme_labels,
                            "reference" = reference_levels,
                            "intercept" = intercept_interpretations,
                            "orthogonal" = orthogonal_contrasts,
                            "dropped_trends" = dropped_trends,
                            "explicitly_set" = FALSE)
  glimpse
}

.cols_where <- function(model_data, fx, use.names = FALSE, return.names = FALSE) {
  cnames <- colnames(model_data)
  cols <- vapply(cnames,
                 function(x) fx(model_data[[x]]),
                 FUN.VALUE = TRUE,
                 USE.NAMES = use.names)
  if (return.names)
    return(cnames[cols])
  cols
  }

.warn_if_nondefault <- function(contrasts, factor_names, factor_sizes, which_ordered) {
  indices <- seq_along(factor_sizes)
  factor_sizes <- unname(factor_sizes)
  ord_fx <- str2lang(options('contrasts')[[1]][["ordered"]])
  unord_fx <- str2lang(options('contrasts')[[1]][["unordered"]])

  same_as_default <-
    vapply(indices,
           function(i) {
             contr_mat <- contrasts[[i]]
             if (which_ordered[i])
               default_mat <- eval(as.call(c(ord_fx, factor_sizes[i])))
             else
               default_mat <- eval(as.call(c(unord_fx, factor_sizes[i])))

             # Check if factor's contrast matrix is same as default settings
             all(round(contr_mat - default_mat, 4) == 0)
           },
           TRUE)

  if (sum(same_as_default) == length(factor_names))
    return(invisible(1))

  unord_str <- crayon::blue(as.character(unord_fx))
  ord_str <- crayon::red(as.character(ord_fx))
  names(which_ordered) <- factor_names
  nondefaults <- vapply(factor_names[!same_as_default],
                        function(x)
                          ifelse(which_ordered[x], crayon::red(x), crayon::blue(x)),
                        "char")
  nondefaults <- paste(" - ", nondefaults, sep = "") |> paste(collapse = "\n")

  warning(glue::glue("These factors not explicitly set and do not use default {unord_str} or {ord_str}. Glimpse table may be unreliable.
             {nondefaults}"))

}

.get_dropped_trends <- function(params, formulas) {
  vapply(seq_along(params),
         function(i){
           trends <- eval(params[[i]][['drop_trends']],
                          rlang::get_env(formulas[[i]]))
           # trends is NA if nothing was passed
           if (NA %in% trends)
             return(NA_character_)
           paste(trends, collapse = ",")
         },
         "char")
}

.get_scheme_labels <- function(params, formulas) {
  vapply(seq_along(params), \(i) {
    scheme <- deparse1(params[[i]][["code_by"]]) # code_by param is a symbol or NA
    # If it's a matrix call then it's taken to be custom contrasts
    if (grepl("^matrix\\(", scheme))
      return("custom")

    # If it's a function name like contr.poly then use the name of the function
    function_used <- is.function(get(scheme,
                                     rlang::get_env(formulas[[i]])))
    if (function_used)
      return(scheme)

    # Else it's a variable name, which is taken to be custom
    return("custom")
  },
  FUN.VALUE = "char",
  USE.NAMES = FALSE
  )
}

.get_reference_levels <- function(contrast_list, params, formulas) {
  reference_levels <- vapply(seq_along(params),
                             function(i){
                               ref_level <- params[[i]][["reference_level"]]

                               if (!is.symbol(ref_level) && is.na(ref_level))
                                 return(NA_character_)

                               # Will evaluate variables and syntactic literals accordingly
                               as.character(eval(ref_level,
                                                 rlang::get_env(formulas[[i]])))
                             },
                             "char")

  # If a reference level wasn't specified, try to figure it out from the matrix
  for (i in seq_along(reference_levels)) {
    if (is.na(reference_levels[i])) {
      intuition <- .intuit_reference_level(contrast_list[[i]],
                                           rownames(contrast_list[[i]]))
      if (!is.na(intuition))
        reference_levels[[i]] <- intuition
    }
  }
  reference_levels
}

.intuit_reference_level <- function(contr_mat, factor_levels) {
  # Dropped factors entails polynomials, which dont have a reference level
  if (ncol(contr_mat) < (length(factor_levels) - 1))
    return(NA)

  hyp_mat <- .contrasts_to_hypotheses(contr_mat, nrow(contr_mat))

  # Matrix[,-1] gets converted to a vector if there are only two levels
  level_matrix <- hyp_mat[,-1]
  if (length(level_matrix) == 2)
    level_matrix <- matrix(level_matrix, nrow = 2)

  # Reference level should be the one with an all negative row
  which_is_ref <- apply(level_matrix, 1, function(x) all(x < 0))
  if (sum(which_is_ref) != 1)
    return(NA)

  factor_levels[which_is_ref]
}

