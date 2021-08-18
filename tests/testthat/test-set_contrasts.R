test_that("set contrast equivalent to manual contrast codes", {
  tst_df <- set_contrasts(dplyr::mutate(mtcars, gear = factor(gear)),
                          gear ~ helmert_code,
                          cyl ~ contr.sum)

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

test_that("Ignoring dropped levels in orthogonal polynomial contrasts", {

  expect_warning(aaa <- set_contrasts(mtcars, carb ~ contr.poly - 4:6),
                 regexp = r"(Cannot drop trends .+ formula\.)")
  expect_warning(aaa <- set_contrasts(mtcars, carb ~ contr.poly - 4:6, gear ~ contr.poly - 2:3),
                 regexp = r"(Cannot drop trends .+ formulas\.)")
  expect_equal(contrasts(aaa$carb), contr.poly(6), ignore_attr = TRUE)
  expect_equal(contrasts(aaa$gear), contr.poly(3), ignore_attr = TRUE)

})
