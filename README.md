
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ao <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/loelschlaeger/ao/workflows/R-CMD-check/badge.svg)](https://github.com/loelschlaeger/ao/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version-last-release/ao)](https://www.r-pkg.org/badges/version-last-release/ao)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/ao)](https://cranlogs.r-pkg.org/badges/grand-total/ao)
<!-- badges: end -->

The goal of ao is alternating optimization of (high-dimensional)
functions.

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

## How to get started?

1.  Define a function `f` that you want to optimize.

2.  Set `npar` equal to the number of parameters of `f`.

3.  Group the parameter indices of `f` into the list `groups`, which
    determines the groups in which parameters get optimized.

4.  Define the vector `sequence`, which determines the sequence in which
    the parameter groups get optimized.

5.  Optionally define the vector `initial` of initial parameter values.
    If not supplied, they get drawn from a standard normal distribution.

6.  Set `minimize = TRUE` for minimizing `f` (the default) or
    `minimize = FALSE` for maximizing `f`.

7.  Set `progress = TRUE` for showing optimization progress. Per
    default, `progress = FALSE`.

8.  Call `ao` with the parameters defined above.

## Example

``` r
library(ao)
#> Thanks for using ao version 0.1.4, happy alternating optimization!
#> See https://loelschlaeger.github.io/ao for help.
#> Type 'citation("ao")' for citing this R package.
ao(f = function(x) 3*x[1]^2 + 2*x[1]*x[2] + x[2]^2 - 5*x[1] + 2,
   npar = 2,
   groups = list(1,2),
   sequence = rep(c(1,2),10))
#> Alternating optimization
#> Minimum value: -1.125 
#> Minimum at: 1.249965 -1.249965 
#> computation time: 0.04 seconds
```
