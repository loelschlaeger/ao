# ao

[![CRAN status](https://www.r-pkg.org/badges/version-last-release/ao)](https://www.r-pkg.org/badges/version-last-release/ao)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/ao)](https://cranlogs.r-pkg.org/badges/grand-total/ao)

👉 Alternating optimization of high-dimensional functions..

💬 Found a bug? Request a feature? Please [tell us](https://github.com/loelschlaeger/ao/issues)!

📝 In R, type `citation("ao")` for citing this package in publications.

## How to get started?

1. Define a function `f` that you want to get optimized.

2. Set `npar` equal to the number of parameters of `f`.

3. Group the parameter indices of `f` into the list `groups`, which determines the groups in which parameters get optimized.

4. Define the vector `sequence`, which determines the sequence in which the parameter groups get optimized.

5. Optionally define the vector `initial` of initial parameter values. If not supplied, they are randomly drawn.

6. Set `minimize = TRUE` for minimizing `f` (the default) or `minimize = FALSE` for maximizing `f`.

7. Set `progress = TRUE` for showing optimization progress. Per default, `progress = FALSE`. 

8. Call `ao` with the parameters defined above.

## Example
```r
ao(f = function(x) 3*x[1]^2 + 2*x[1]*x[2] + x[2]^2 - 5*x[1] + 2,
   npar = 2,
   groups = list(1,2),
   sequence = rep(c(1,2),10))
```

