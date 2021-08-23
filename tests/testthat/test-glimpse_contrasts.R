test_that("Intuit reference level works", {
  expect_equal(.intuit_reference_level(contr.poly(6), 1:6), NA)
  expect_equal(.intuit_reference_level(contr.treatment(2), 1:2), 1)
  expect_equal(.intuit_reference_level(contr.SAS(6), 1:6), 6)
  expect_equal(.intuit_reference_level(scaled_sum_code(6), 1:6), 6)
  expect_equal(.intuit_reference_level(helmert_code(6), 1:6), 6)
  expect_equal(.intuit_reference_level(reverse_helmert_code(6), 1:6), 1)
  expect_equal(.intuit_reference_level(contr.sum(6), 1:6), 6)
})

test_that("Intercept interpretation works", {
  expect_equal(interpret_intercept(contr.treatment(5)), "mean(1)", ignore_attr = TRUE)
  expect_equal(interpret_intercept(scaled_sum_code(5)), "grand mean", ignore_attr = TRUE)
  expect_equal(interpret_intercept(helmert_code(5)), "grand mean", ignore_attr = TRUE)
  expect_equal(interpret_intercept(contr.poly(5)[,1:3]), "grand mean", ignore_attr = TRUE)
})

test_that("Glimpse works", {
  tstdf <- mtcars
  tstdf$cyl <- factor(tstdf$cyl)
  tst <- glimpse_contrasts(tstdf,
                           carb ~ contr.poly - 3:5,
                           gear ~ scaled_sum_code + 5,
                           verbose = FALSE)
  expect_equal(tst$factor, c('carb','gear','cyl'))
  expect_equal(tst$n_levels, c(6, 3, 3), ignore_attr = TRUE) # need unname
  expect_equal(tst$scheme, c('contr.poly','scaled_sum_code','contr.treatment'), ignore_attr = TRUE) # need unname
  expect_equal(tst$reference, c(NA, '5', '4'), ignore_attr = TRUE) # need unname
  expect_equal(tst$intercept, c('grand mean', 'grand mean', 'mean(4)'), ignore_attr = TRUE)
  expect_equal(tst$orthogonal, c(TRUE, FALSE, FALSE))
  expect_equal(tst$dropped_trends, c("3,4,5",NA,NA))
  expect_equal(tst$explicitly_set, c(TRUE,TRUE,FALSE))
})
