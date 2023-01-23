
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Alternating Optimization in R with {ao}

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version-last-release/ao)](https://www.r-pkg.org/badges/version-last-release/ao)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/last-month/ao)](https://cranlogs.r-pkg.org/badges/grand-total/ao)
[![R-CMD-check](https://github.com/loelschlaeger/ao/workflows/R-CMD-check/badge.svg)](https://github.com/loelschlaeger/ao/actions)
[![Codecov test
coverage](https://codecov.io/gh/loelschlaeger/ao/branch/main/graph/badge.svg)](https://app.codecov.io/gh/loelschlaeger/ao?branch=main)
<!-- badges: end -->

This R package implements alternating optimization, which is an
iterative procedure for optimizing some function jointly over all
parameters by alternating restricted optimization over individual
parameter subsets. For more details, see the [package
vignette](https://cran.r-project.org/package=ao/vignettes/ao.html).

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

The following lines perform alternating optimization of the
[Himmelblau’s
function](https://en.wikipedia.org/wiki/Himmelblau%27s_function),
separately for $x_1$ and $x_2$, with the parameter restrictions
$-5 \leq x_1, x_2 \leq 5$:

``` r
library("ao")
himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
ao(
  f = himmelblau, p = c(0,0), partition = list(1, 2),
  base_optimizer = optimizer_optim(lower = -5, upper = 5, method = "L-BFGS-B")
)
#> $optimum
#> [1] 1.940035e-12
#> 
#> $estimate
#> [1]  3.584428 -1.848126
#> 
#> $sequence
#>    iteration partition         time       p1        p2
#> 1          0         0 0.0000000000 0.000000  0.000000
#> 2          1         1 0.0184102058 3.395691  0.000000
#> 3          1         2 0.0002691746 3.395691 -1.803183
#> 4          2         1 0.0002059937 3.581412 -1.803183
#> 5          2         2 0.0001771450 3.581412 -1.847412
#> 6          3         1 0.0002670288 3.584381 -1.847412
#> 7          3         2 0.0001451969 3.584381 -1.848115
#> 8          4         1 0.0001440048 3.584427 -1.848115
#> 9          4         2 0.0001239777 3.584427 -1.848126
#> 10         5         1 0.0001220703 3.584428 -1.848126
#> 11         5         2 0.0001230240 3.584428 -1.848126
#> 
#> $time
#> Time difference of 0.022825 secs
```

## Contact

Have a question, found a bug, request a feature, want to contribute?
[Please file an
issue](https://github.com/loelschlaeger/ao/issues/new/choose).
