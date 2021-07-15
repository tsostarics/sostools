test_that("contrasts to latex works", {
  test_col <- factor(1:7)
  contrasts(test_col) <- contrast_code(test_col, backward_difference_code, set_intercept = 7)
  result <- contrasts_as_latex(test_col)
  comparison <- "\n  \\begin{table}[htb]\n  \\centering\n  \\begin{tabular}{l|cccccc}\n    & 2-1 & 3-2 & 4-3 & 5-4 & 6-5 & 7-6 \\\\\n  \\hline\n  1\t&\t-1\t&\t-1\t&\t-1\t&\t-1\t&\t-1\t&\t-1\\\\\n2\t&\t0\t&\t-1\t&\t-1\t&\t-1\t&\t-1\t&\t-1\\\\\n3\t&\t0\t&\t0\t&\t-1\t&\t-1\t&\t-1\t&\t-1\\\\\n4\t&\t0\t&\t0\t&\t0\t&\t-1\t&\t-1\t&\t-1\\\\\n5\t&\t0\t&\t0\t&\t0\t&\t0\t&\t-1\t&\t-1\\\\\n6\t&\t0\t&\t0\t&\t0\t&\t0\t&\t0\t&\t-1\\\\\n7\t&\t0\t&\t0\t&\t0\t&\t0\t&\t0\t&\t0\n  \\end{tabular}\n  \\end{table}"
  expect_equal(result, comparison)
  })
