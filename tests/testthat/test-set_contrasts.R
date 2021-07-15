test_that("set contrast equivalent to manual contrast codes", {
  tst_df <- dplyr::mutate(mtcars, gear = factor(gear))
  tst_df <- set_contrasts(tst_df, gear ~ helmert_code, cyl ~ contr.sum)

  comparison_df <- dplyr::mutate(mtcars, gear = factor(gear), cyl = factor(cyl))
  contrasts(comparison_df$cyl) <- contrast_code(comparison_df$cyl, contr.sum)
  contrasts(comparison_df$gear) <- contrast_code(comparison_df$gear, helmert_code)

  columns_equivalent <-
    vapply(names(tst_df),
         function(x)
           all(tst_df[[x]] == comparison_df[[x]]) & class(tst_df[[x]]) == class(comparison_df[[x]]),
         TRUE)

  expect_true(all(columns_equivalent))

  })
