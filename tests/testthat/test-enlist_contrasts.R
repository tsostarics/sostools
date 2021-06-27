test_that(".check_remaining_factors works", {
  tst_data <-
    tibble::tribble(
      ~two, ~three, ~four,
      "a",    "a",   "a",
      "b",    "b",   "b",
      "a",    "c",   "c",
      "b",    "a",   "d"
    ) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), factor))

  expect_message(.check_remaining_factors(tst_data, "a"),
                 "You didn't set these factors, expect dummy coding: two, three, four")

})

test_that("enlist_contrasts names works", {
  tst_data <-
    tibble::tribble(
      ~two, ~three, ~four,
      "a",    "a",   "a",
      "b",    "b",   "b",
      "a",    "c",   "c",
      "b",    "a",   "d"
    ) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), factor))

  expect_equal(names(enlist_contrasts(tst_data, two~contr.sum, three~scaled_sum_code, four~contr.poly)),
  c("two", "three", "four"))
})

test_that("enlist_contrasts column not found", {
  tst_data <-
    tibble::tribble(
      ~two, ~three, ~four,
      "a",    "a",   "a",
      "b",    "b",   "b",
      "a",    "c",   "c",
      "b",    "a",   "d"
    ) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), factor))

  expect_error(enlist_contrasts(tst_data, foo~contr.sum),regexp = "foo not found in model data")
})
