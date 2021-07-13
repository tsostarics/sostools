test_that("multiplication works", {
  mdl <- lm(mpg ~ cyl * gear, data = mtcars) # Create model
  coefs <- enlist_coefs(mdl) # Create coefficients
  get_coef1 <- make_coef_getter(mdl) # Using the model object directly
  get_coef2 <- make_coef_getter(coefs) # Using the list of coefficients we made
  # All are equivalent
  expect_equal(
    get_coef("cyl:gear", coefs),
    get_coef1("cyl:gear")
  )

  expect_equal(get_coef1("cyl:gear"),get_coef2("cyl:gear"))
})
