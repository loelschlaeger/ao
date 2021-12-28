
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ao: alternating optimization <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/loelschlaeger/ao/workflows/R-CMD-check/badge.svg)](https://github.com/loelschlaeger/ao/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version-last-release/ao)](https://www.r-pkg.org/badges/version-last-release/ao)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/ao)](https://cranlogs.r-pkg.org/badges/grand-total/ao)
[![Codecov test
coverage](https://codecov.io/gh/loelschlaeger/ao/branch/main/graph/badge.svg)](https://app.codecov.io/gh/loelschlaeger/ao?branch=main)
<!-- badges: end -->

This package performs alternating optimization, which is an iterative
procedure for optimizing some function jointly over all variables by
alternating restricted optimization over individual variable subsets.

See the [vignette](https://loelschlaeger.github.io/ao/articles/ao.html)
for more details.

## Installation

You can install the released version of ao from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ao")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("loelschlaeger/ao")
```

## Example

This example is explained in detail in the
[vignette](https://loelschlaeger.github.io/ao/articles/ao.html).

``` r
library(ao)
#> Thanks for using ao version 0.1.4.9000, happy alternating optimization!
#> See https://loelschlaeger.github.io/ao for help.
#> Type 'citation("ao")' for citing this R package.
valley <- function(x) {
  cons <- c(1.003344481605351, -3.344481605351171e-03)
  n <- length(x)
  f <- rep(0, n)
  j <- 3 * (1:(n/3))
  jm2 <- j - 2
  jm1 <- j - 1
  f[jm2] <- (cons[2]*x[jm2]^3 + cons[1]*x[jm2]) * exp(-(x[jm2]^2)/100) - 1
  f[jm1] <- 10 * (sin(x[jm2]) - x[jm1])
  f[j] <- 10 * (cos(x[jm2]) - x[j])
  sum(f*f)
}
f <- set_f(f = valley, npar = 9, lower = 0, upper = 10)
ao(f = f, partition = list(1, 2, 3, 4, 5, 6, 7, 8, 9), initial = 0, iterations = 1e10, plot = FALSE)
#> Optimum value: 5.315569e-12 
#> Optimum at: 1.010331 0.8470081 0.53158 1.010331 0.8470081 0.53158 1.010331 0.8470081 0.53158 
#> Optimization time: 0.7 seconds
```
