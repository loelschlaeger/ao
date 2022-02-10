
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ao: Alternating Optimization <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/loelschlaeger/ao/workflows/R-CMD-check/badge.svg)](https://github.com/loelschlaeger/ao/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version-last-release/ao)](https://www.r-pkg.org/badges/version-last-release/ao)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/ao)](https://cranlogs.r-pkg.org/badges/grand-total/ao)
[![Codecov test
coverage](https://codecov.io/gh/loelschlaeger/ao/branch/main/graph/badge.svg)](https://app.codecov.io/gh/loelschlaeger/ao?branch=main)
<!-- badges: end -->

This package implemented alternating optimization, which is an iterative
procedure for optimizing some function jointly over all parameters by
alternating restricted optimization over individual parameter subsets.

See the [vignette](https://loelschlaeger.github.io/ao/articles/ao.html)
for more details on the method.

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

``` r
library(ao)
#> Thanks for using ao version 0.2.1, happy alternating optimization!
#> See https://loelschlaeger.github.io/ao for help.
#> Type 'citation("ao")' for citing this R package.
himmelblau <- function(x) (x[1]^2+x[2]-11)^2 + (x[1]+x[2]^2-7)^2
f <- set_f(f = himmelblau, npar = 2, lower = -5, upper = 5)
ao(f = f, partition = list(1, 2), progress = FALSE, plot = FALSE)
#> Optimum value: 1.940035e-12 
#> Optimum at: 3.584428 -1.848126 
#> Optimization time: 0.48 seconds
```

This example is explained in detail
[here](https://loelschlaeger.github.io/ao/articles/ao.html#application).
