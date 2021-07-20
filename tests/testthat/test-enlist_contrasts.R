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
                 "You didn't set these factors, expect dummy coding: two three four")

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

  expect_error(enlist_contrasts(tst_data, foo~contr.sum),regexp = "Can't subset columns")
})

test_that("setting both reference and intercept works", {

  my_df <- mtcars
  my_df$gear = factor(my_df$gear)
  my_df$carb = factor(my_df$carb)
  expect_equal(
    enlist_contrasts(my_df,
                   gear ~ contr.treatment,
                   carb ~ contr.treatment)[[2L]],
    enlist_contrasts(my_df,
                     gear ~ contr.treatment,
                     carb ~ scaled_sum_code + 1 * 1)[[2L]]
  )

}
)

test_that("matrix passing works", {
  my_df <- mtcars
  my_df$gear = factor(my_df$gear)
  my_df$carb = factor(my_df$carb)

  reference <- enlist_contrasts(my_df,
                                gear ~ contr.sum + 4,
                                carb ~ forward_difference_code)[[1L]]
  # Raw matrix
  expect_equal(
    enlist_contrasts(my_df,
                     gear ~ matrix(c(1,-1,0,0,-1,1), nrow = 3),
                     carb ~ forward_difference_code)[[1L]],
    reference
    )

})
