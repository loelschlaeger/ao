
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Alternating Optimization <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ao)](https://CRAN.R-project.org/package=ao)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/last-month/ao)](https://CRAN.R-project.org/package=ao)
[![R-CMD-check](https://github.com/loelschlaeger/ao/workflows/R-CMD-check/badge.svg)](https://github.com/loelschlaeger/ao/actions)
[![Codecov test
coverage](https://codecov.io/gh/loelschlaeger/ao/branch/main/graph/badge.svg)](https://app.codecov.io/gh/loelschlaeger/ao?branch=main)
<!-- badges: end -->

The `{ao}` package implements alternating optimization in `R`.

Alternating optimization is an iterative procedure which optimizes a
function jointly over all parameters by alternately performing
restricted optimization over individual parameter subsets.

For additional details on the method, please refer to the [package
vignette](https://loelschlaeger.de/ao/articles/ao.html).

## Installation

You can install the released version from
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

The following is a simple example to perform alternating optimization of
the [Himmelblau’s
function](https://en.wikipedia.org/wiki/Himmelblau%27s_function),
separately for $x_1$ and $x_2$, with the parameter restrictions
$-5 \leq x_1, x_2 \leq 5$:

``` r
library("ao")
himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
ao(
  f = himmelblau, p = c(0, 0), partition = list(1, 2),
  base_optimizer = optimizer_optim(lower = -5, upper = 5, method = "L-BFGS-B")
)
#> $value
#> [1] 1.940035e-12
#> 
#> $estimate
#> [1]  3.584428 -1.848126
#> 
#> $sequence
#>    iteration partition        value      seconds       p1        p2
#> 1          0        NA 1.700000e+02 0.000000e+00 0.000000  0.000000
#> 2          1         1 1.327270e+01 2.636909e-03 3.395691  0.000000
#> 3          1         2 1.743666e+00 1.060963e-04 3.395691 -1.803183
#> 4          2         1 2.847292e-02 8.106232e-05 3.581412 -1.803183
#> 5          2         2 4.687472e-04 6.794930e-05 3.581412 -1.847412
#> 6          3         1 7.368063e-06 9.703636e-05 3.584381 -1.847412
#> 7          3         2 1.157612e-07 5.507469e-05 3.584381 -1.848115
#> 8          4         1 1.900153e-09 5.006790e-05 3.584427 -1.848115
#> 9          4         2 4.221429e-11 4.291534e-05 3.584427 -1.848126
#> 10         5         1 3.598278e-12 4.196167e-05 3.584428 -1.848126
#> 11         5         2 1.940035e-12 4.100800e-05 3.584428 -1.848126
#> 
#> $seconds
#> [1] 0.003220081
```

## Contact

Have a question, found a bug, request a feature, want to contribute?
[Please file an
issue](https://github.com/loelschlaeger/ao/issues/new/choose).
