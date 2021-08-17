test_that("Messaging when there are remaining factors works", {
  tst_data <-
    tibble::tribble(
      ~two, ~three, ~four,
      "a",    "a",   "a",
      "b",    "b",   "b",
      "a",    "c",   "c",
      "b",    "a",   "d"
    ) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), factor))
  tst_data$three <- ordered(tst_data$three)
  expect_message(.msg_if_remaining_factors(tst_data, "a"),
                 regexp= "You didn't set these factors, expect .+: two three four")

})

test_that("Messaging if user tries to reset ordered factors works", {
  my_df <- mtcars
  my_df$gear <-  ordered(my_df$gear)

  expect_message(enlist_contrasts(my_df, gear ~ scaled_sum_code),
                 regexp = "These factors are ordered")
})
