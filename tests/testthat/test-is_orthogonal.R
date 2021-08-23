test_that("Orthogonality checks are correct", {
  tst_df <- tibble::tibble(a = factor(1:5),
                 b = a,
                 c = a,
                 d = a,
                 e = a,
                 f = a,
                 g = a,
                 h = a)
  orth_vector <- enlist_contrasts(tst_df,
                   a ~ contr.treatment,
                   b ~ scaled_sum_code,
                   c ~ contr.poly,
                   d ~ helmert_code,
                   e ~ backward_difference_code,
                   f ~ backward_difference_code * 3,
                   g ~ reverse_helmert_code,
                   h ~ reverse_helmert_code * 3) %>%
    is_orthogonal()

  references <-
    c("a" = FALSE,
    "b" = FALSE,
    "c" = TRUE,
    "d" = TRUE,
    "e" = FALSE,
    "f" = FALSE,
    "g" = TRUE,
    "h" = TRUE)

  expect_equal(orth_vector, references)

})
