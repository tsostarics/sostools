test_that("centered checks work", {
  expect_true(is_centered(scaled_sum_code(2)))
  expect_true(is_centered(scaled_sum_code(4)))
  expect_true(is_centered(sum_code(4)))
  expect_true(is_centered(helmert_code(2)))
  expect_true(is_centered(contr.poly(2)))
  expect_false(is_centered(contr.treatment(2)))
})
