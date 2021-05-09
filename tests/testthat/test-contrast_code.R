test_that("two level factor coding works", {
  tst_data <-
    tibble::tribble(
      ~two, ~three, ~four,
      "a",    "a",   "a",
      "b",    "b",   "b",
      "a",    "c",   "c",
      "b",    "a",   "d"
    ) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), factor))

  expect_equal(contrasts(tst_data$two), contrast_code(tst_data$two, matrix(c(0,1))))
  expect_equal(colnames(contrast_code(tst_data$two, matrix(c(.5, -.5)))), "a")
  expect_equal(colnames(contrast_code(tst_data$two, matrix(c(1,0)))),"a")
  expect_equal(colnames(contrast_code(tst_data$two, matrix(c(0,1)))),"b")
})

test_that("three level factor coding works", {
  tst_data <-
    tibble::tribble(
      ~two, ~three, ~four,
      "a",    "a",   "a",
      "b",    "b",   "b",
      "a",    "c",   "c",
      "b",    "a",   "d"
    ) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), factor))

  expect_equal(colnames(contrast_code(tst_data$three, matrix(c(-1/3, -1/3, 2/3, 2/3, -1/3, -1/3), nrow=3))),
               c("c","a"))
  expect_equal(matrix(c(-1/3, -1/3, 2/3, 2/3, -1/3, -1/3),nrow=3),
               contrast_code(tst_data$three, matrix(c(-1/3, -1/3, 2/3, 2/3, -1/3, -1/3),nrow=3)),
               ignore_attr = TRUE)
})

test_that("four level helmert coding works", {
  tst_data <-
    tibble::tribble(
      ~two, ~three, ~four,
      "a",    "a",   "a",
      "b",    "b",   "b",
      "a",    "c",   "c",
      "b",    "a",   "d"
    ) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), factor))

  expect_equal(colnames(contrast_code(tst_data$four, matrix(c(-1/4, -1/4, -1/4, 3/4,
                                                              -1/3, -1/3, 2/3, 0,
                                                              -1/2, 1/2, 0, 0),
                                                            nrow = 4))),
               c("d","c","b"))
  expect_equal(contrast_code(tst_data$four, matrix(c(-1/4, -1/4, -1/4, 3/4,
                                                     -1/3, -1/3, 2/3, 0,
                                                     -1/2, 1/2, 0, 0),
                                                   nrow = 4)),
               matrix(c(-1/4, -1/4, -1/4, 3/4,
                        -1/3, -1/3, 2/3, 0,
                        -1/2, 1/2, 0, 0),
                      nrow = 4),
               ignore_attr = TRUE)
})
