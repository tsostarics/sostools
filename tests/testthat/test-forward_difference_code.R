test_that("backward difference labels work", {
  tst_data <-
    tibble::tribble(
      ~two, ~three, ~four,
      "a",    "a",   "a",
      "b",    "b",   "b",
      "a",    "c",   "c",
      "b",    "a",   "d"
    ) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), factor))

  expect_equal(dimnames(contrast_code(tst_data$four, backward_difference_code(4))),
               list(c('a', 'b', 'c', 'd'),
                    c('b-a', 'c-b', 'd-c')))
})

test_that("forward difference labels work", {
  tst_data <-
    tibble::tribble(
      ~two, ~three, ~four,
      "a",    "a",   "a",
      "b",    "b",   "b",
      "a",    "c",   "c",
      "b",    "a",   "d"
    ) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), factor))

  expect_equal(dimnames(contrast_code(tst_data$four, forward_difference_code(4))),
               list(c('a', 'b', 'c', 'd'),
                    c('a-b', 'b-c', 'c-d')))
})

test_that("backward difference values correct", {
  expect_equal(backward_difference_code(4),
               matrix(c(-.75, .25, .25, .25,
                        -.5, -.5, .5, .5,
                        -.25, -.25, -.25, .75), nrow=4)
  )
})

test_that("forward difference values correct", {
  expect_equal(forward_difference_code(4),
               matrix(rev(c(-.75, .25, .25, .25,
                        -.5, -.5, .5, .5,
                        -.25, -.25, -.25, .75)), nrow=4)
  )
})
