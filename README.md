
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
```

Here we can get the formatted values of a write up easily with in-line R
code. We found no effect of cylinder
(*β* =  − 0.18, *t* =  − 0.09, *s*.*e*. = 2.05, *p* = 0.931) or gear
(*β* = 5.14, *t* = 1.41, *s*.*e*. = 3.63, *p* = 0.168) nor an
interaction
(*β* =  − 0.67, *t* =  − 1.27, *s*.*e*. = 0.53, *p* = 0.213).

## Retain contrast coding labels

``` r
mdl_data <- 
  mtcars %>% 
  as_tibble() %>% 
  mutate(cyl = factor(cyl), 
         twolevel = factor(round(runif(n()),0)))
contrasts(mdl_data$cyl) # 3 level treatment coding
#>   6 8
#> 4 0 0
#> 6 1 0
#> 8 0 1
contrasts(mdl_data$twolevel) # 2 level treatment coding
#>   1
#> 0 0
#> 1 1
contrast_code(mdl_data$twolevel) # 2 level scaled sum coding
#>      1
#> 0 -0.5
#> 1  0.5

# note the matrix orientation for 3 level scaled sum coding
contrasts(mdl_data$cyl) <- contrast_code(mdl_data$cyl,
                                        matrix(c(-1/3, 2/3, -1/3,
                                                 -1/3, -1/3, 2/3),
                                               nrow = 3))

contrasts(mdl_data$cyl) # contrasts now updated with labels retained
#>            6          8
#> 4 -0.3333333 -0.3333333
#> 6  0.6666667 -0.3333333
#> 8 -0.3333333  0.6666667
```
