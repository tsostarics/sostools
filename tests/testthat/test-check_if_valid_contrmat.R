test_that("multiplication works", {
  expect_error(.check_if_valid_contrmat(orth_polynomial_code(5)[,-1]), regexp = "must be square")
})
