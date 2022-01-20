test_that("basic model output works", {
  mdl <- lm(mpg ~ cyl + gear, data = mtcars)
  output <- as.character(summary_as_tex(mdl))
  reference <- "\\begin{table}[htbp]\n\n\\centering\n\\begin{tabular}[t]{lrrrl}\n\\toprule\nTerm & Estimate & SE & $z$ & $p$\\\\\n\\midrule\n(Intercept) & 34.66 & 4.94 & 7.02 & <0.001***\\\\\ncyl & -2.74 & 0.37 & -7.34 & <0.001***\\\\\ngear & 0.65 & 0.90 & 0.72 & 0.477\\\\\n\\bottomrule\n\\end{tabular}\n\\flushleft\n*p\\textless{}0.05; **p\\textless{}0.01; ***p\\textless{}0.001\n\\end{table}"
  expect_equal(output, reference)
})
