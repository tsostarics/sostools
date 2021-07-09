
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

You can also use these with Bayesian models fit with `brms`, which will
give you the *β* estimate as well as the credible interval.

## Contrast coding functionality

See the `contrasts` vignette with `vignette('contrasts', 'sostools')`.
