test_that("enlist coefficients works", {
  tstmdl <- lm(mpg ~ cyl*gear, data = set_contrasts(mtcars, gear ~ scaled_sum_code, verbose = FALSE))
  coefs <- enlist_coefs(tstmdl)
  expect_equal(as.character(coefs$Intercept), r"($(\hat\beta = 37.21, t = 12.55, s.e. = 2.96, p < .001)$)")
  expect_equal(as.character(coefs$`cyl:gear5`), r"($(\hat\beta = -1.37, t = -1.25, s.e. = 1.1, p=0.223)$)")
})

test_that("p value adjustment works", {
  tstmdl <- lm(mpg ~ cyl*gear, data = set_contrasts(mtcars, gear ~ scaled_sum_code, verbose = FALSE))
  coefs <- enlist_coefs(tstmdl, correct = "gear")
  expect_equal(as.character(coefs$Intercept), r"($(\hat\beta = 37.21, t = 12.55, s.e. = 2.96, p < .001)$)")
  expect_equal(as.character(coefs$`cyl:gear5`), r"($(\hat\beta = -1.37, t = -1.25, s.e. = 1.1, p=0.508)$)")
})
