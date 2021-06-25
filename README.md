
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sostools

<!-- badges: start -->
<!-- badges: end -->

This is just a collection of functions I use in my research that I use
frequently. Other functions I use infrequently can be found in my
`snippets` repo.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tsostarics/sostools")
```

## Easily reference coefficients in prose

Here is a simple example of referencing the values of a model summary
without needing to hand write or format it yourself. See `?get_coef` for
more information. `make_coef_getter` is a function factory that provides
a shorthand if you have multiple models you’re working with.

``` r
library(sostools)
library(tidyverse)
mdl <- lm(mpg ~ cyl * gear, data = mtcars) # Create model
coefs <- enlist_coefs(mdl) # Create coefficients
get_coef1 <- make_coef_getter(mdl) # Using the model object directly
get_coef2 <- make_coef_getter(coefs) # Using the list of coefficients we made
get_coef("cyl:gear", coefs)
#> $(\beta = -0.67, t = -1.27, s.e. = 0.53, p=0.213)$
get_coef1("cyl:gear")
#> $(\beta = -0.67, t = -1.27, s.e. = 0.53, p=0.213)$
get_coef2("cyl:gear")
#> $(\beta = -0.67, t = -1.27, s.e. = 0.53, p=0.213)$
```

Here we can get the formatted values of a write up easily with in-line R
code. You could say something like: We found no significant effect of
cylinder (*β* =  − 0.18, *t* =  − 0.09, *s*.*e*. = 2.05, *p* = 0.931) or
gear (*β* = 5.14, *t* = 1.41, *s*.*e*. = 3.63, *p* = 0.168) nor a
significant interaction between the two
(*β* =  − 0.67, *t* =  − 1.27, *s*.*e*. = 0.53, *p* = 0.213).

## Retain contrast coding labels

Here’s a test data set with some factors that we might want to apply
particular contrasts to.

``` r
mdl_data <- 
  mtcars %>% 
  as_tibble() %>% 
  mutate(cyl = factor(cyl), 
         twolevel = round(runif(n()),0),
         twolevel = ifelse(twolevel == 1, "a", "b"),
         twolevel = factor(twolevel))
contrasts(mdl_data$cyl) # 3 level treatment coding
#>   6 8
#> 4 0 0
#> 6 1 0
#> 8 0 1
contrasts(mdl_data$twolevel) # 2 level treatment coding
#>   b
#> a 0
#> b 1
contrast_code(mdl_data$twolevel) # 2 level scaled sum coding
#>      b
#> a -0.5
#> b  0.5
```

First, we can specify the contrast matrix manually using
`contrast_code()` or `manual_code()`. The former will invoke the latter
if it detects a matrix.

``` r
# You can specify contrast matrix manually
contrasts(mdl_data$cyl) <- contrast_code(mdl_data$cyl,
                                        matrix(c(2/3, -1/3, -1/3,
                                                 -1/3, -1/3, 2/3),
                                               nrow = 3))

# contrasts now updated with labels retained and proper comparison labels
# set depending on what the reference level is (here, 6 is the reference)
contrasts(mdl_data$cyl) 
#>            4          8
#> 4  0.6666667 -0.3333333
#> 6 -0.3333333 -0.3333333
#> 8 -0.3333333  0.6666667

# also works for polynomial coding
contrast_code(mdl_data$cyl, contr.poly(3))
#>              .L         .Q
#> 4 -7.071068e-01  0.4082483
#> 6 -7.850462e-17 -0.8164966
#> 8  7.071068e-01  0.4082483
```

Because manually specifying matrices is time consuming for some common
contrast schemes, you can also pass a function that generates contrasts
to `contrast_code()` which will invoke `functional_code()`. A series of
helpers are included in this package, but they also work with R’s built
in `contr.___` functions. When applicable with sum and dummy coding, you
can pass in a desired reference level. Coding schemes that require a
specific level order, such as forward/backward difference or helmert
coding, should be set before applying contrasts.

``` r
contrast_code(mdl_data$cyl, scaled_sum_code)
#>            6          8
#> 4 -0.3333333 -0.3333333
#> 6  0.6666667 -0.3333333
#> 8 -0.3333333  0.6666667
contrast_code(mdl_data$cyl, scaled_sum_code, reference = "6")
#>            4          8
#> 4  0.6666667 -0.3333333
#> 6 -0.3333333 -0.3333333
#> 8 -0.3333333  0.6666667
contrast_code(mdl_data$cyl, forward_difference_code)
#>          4-6        6-8
#> 4  0.6666667  0.3333333
#> 6 -0.3333333  0.3333333
#> 8 -0.3333333 -0.6666667
contrast_code(mdl_data$cyl, backward_difference_code)
#>          6-4        8-6
#> 4 -0.6666667 -0.3333333
#> 6  0.3333333 -0.3333333
#> 8  0.3333333  0.6666667
contrast_code(mdl_data$cyl, helmert_code)
#>            4    6
#> 4  0.6666667  0.0
#> 6 -0.3333333  0.5
#> 8 -0.3333333 -0.5
contrast_code(mdl_data$cyl, reverse_helmert_code)
#>      6          8
#> 4 -0.5 -0.3333333
#> 6  0.5 -0.3333333
#> 8  0.0  0.6666667
contrast_code(mdl_data$cyl, contr.poly)
#>              .L         .Q
#> 4 -7.071068e-01  0.4082483
#> 6 -7.850462e-17 -0.8164966
#> 8  7.071068e-01  0.4082483
contrast_code(mdl_data$cyl, contr.sum)
#>    6  8
#> 4 -1 -1
#> 6  1  0
#> 8  0  1
contrast_code(mdl_data$cyl, contr.treatment)
#>   6 8
#> 4 0 0
#> 6 1 0
#> 8 0 1
contrast_code(mdl_data$cyl, contr.treatment, reference = "6")
#>   4 8
#> 4 1 0
#> 6 0 0
#> 8 0 1
```
