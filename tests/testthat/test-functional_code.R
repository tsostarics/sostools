test_that("two level functional coding work", {
  tst_data <-
    tibble::tribble(
      ~two, ~three, ~four,
      "a",    "a",   "a",
      "b",    "b",   "b",
      "a",    "c",   "c",
      "b",    "a",   "d"
    ) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), factor))

  expect_equal(functional_code(tst_data$two, contr.sum, reference_level = "b"),
               manual_code(tst_data$two, contr.sum(2)))
  expect_equal(functional_code(tst_data$two, contr.sum, reference_level = "a"),
               manual_code(tst_data$two, -contr.sum(2)))
})

test_that("four level functional coding work", {
  tst_data <-
    tibble::tribble(
      ~two, ~three, ~four,
      "a",    "a",   "a",
      "b",    "b",   "b",
      "a",    "c",   "c",
      "b",    "a",   "d"
    ) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), factor))

  expect_equal(functional_code(tst_data$four, contr.poly),
               manual_code(tst_data$four, contr.poly(4)))
  expect_equal(functional_code(tst_data$four, reverse_helmert_code),
               manual_code(tst_data$four, reverse_helmert_code(4)))
})
