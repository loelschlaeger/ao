# ao: Alternating Optimization

Implementation of an iterative process that optimizes a function by
alternately performing restricted optimization over parameter subsets.
Instead of solving one joint optimization problem, alternating
optimization breaks it into smaller sub-problems. This approach can make
optimization feasible when joint optimization is too difficult.

## See also

Useful links:

- <https://loelschlaeger.de/ao/>

- <https://github.com/loelschlaeger/ao/>

- Report bugs at <https://github.com/loelschlaeger/ao/issues>

## Author

**Maintainer**: Lennart Oelschläger <oelschlaeger.lennart@gmail.com>
([ORCID](https://orcid.org/0000-0001-5421-9313))

Other contributors:

- Siddhartha Chib <chib@wustl.edu> \[contributor\]

## Examples

``` r
# Example 1: Minimization of Himmelblau's function --------------------------

himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
ao(f = himmelblau, initial = c(0, 0))
#> $estimate
#> [1]  3.584428 -1.848126
#> 
#> $value
#> [1] 9.606386e-12
#> 
#> $details
#>    iteration        value       p1        p2 b1 b2     seconds
#> 1          0 1.700000e+02 0.000000  0.000000  0  0 0.000000000
#> 2          1 1.327270e+01 3.395691  0.000000  1  0 0.043577433
#> 3          1 1.743664e+00 3.395691 -1.803183  0  1 0.009064674
#> 4          2 2.847290e-02 3.581412 -1.803183  1  0 0.007369995
#> 5          2 4.687468e-04 3.581412 -1.847412  0  1 0.012878418
#> 6          3 7.368057e-06 3.584381 -1.847412  1  0 0.005236626
#> 7          3 1.164202e-07 3.584381 -1.848115  0  1 0.043940783
#> 8          4 1.893311e-09 3.584427 -1.848115  1  0 0.004272938
#> 9          4 9.153860e-11 3.584427 -1.848124  0  1 0.003256559
#> 10         5 6.347425e-11 3.584428 -1.848124  1  0 0.003205776
#> 11         5 9.606386e-12 3.584428 -1.848126  0  1 0.003211975
#> 
#> $seconds
#> [1] 0.1360152
#> 
#> $stopping_reason
#> [1] "change in function value between 1 iteration is < 1e-06"
#> 

# Example 2: Maximization of 2-class Gaussian mixture log-likelihood --------

normal_mixture_loglik_uc = function(mu, logsd, eta, data) {
  sd <- exp(logsd)
  e <- exp(eta[1])
  den <- 1 + e
  q1 <- e / den
  q2 <- 1 / den
  l1 <- log(q1) + dnorm(data, mu[1], sd[1], log = TRUE)
  l2 <- log(q2) + dnorm(data, mu[2], sd[2], log = TRUE)
  m <- pmax(l1, l2)
  sum(m + log(exp(l1 - m) + exp(l2 - m)))
}

set.seed(123)

data <- datasets::faithful$eruptions

fit <- ao(
  f = normal_mixture_loglik_uc,
  initial = c(mean(data) + c(-1, 1), rep(log(sd(data)), 2), 0),
  target = c("mu", "logsd", "eta"),
  npar = c(2, 2, 1),
  data = data,
  partition = "random",
  base_optimizer = optimizeR::Optimizer$new("ucminf::ucminf"),
  minimize = FALSE,
  add_details = FALSE
)

(muhat <- fit$estimate_split$mu)
#> [1] 2.018607 4.273341
(sdhat <- exp(fit$estimate_split$logsd))
#> [1] 0.2356211 0.4370635
e <- exp(fit$estimate_split$eta)
den <- 1 + e
(qhat <- c(e / den, 1 / den))
#> [1] 0.3484044 0.6515956

# Example 3: Constrained Optimization in the Setting of Example 2 -----------

# target arguments:
# - class means mu (2, unrestricted)
# - class standard deviations sd (2, must be non-negative)
# - class proportion lambda (only 1 for identification, must be in [0, 1])

normal_mixture_loglik <- function(mu, sd, lambda, data) {
  c1 <- lambda * dnorm(data, mu[1], sd[1])
  c2 <- (1 - lambda) * dnorm(data, mu[2], sd[2])
  sum(log(c1 + c2))
}

set.seed(123)

ao(
  f = normal_mixture_loglik,
  initial = runif(5),
  target = c("mu", "sd", "lambda"),
  npar = c(2, 2, 1),
  data = datasets::faithful$eruptions,
  partition = list("sequential", "random", "none"),
  minimize = FALSE,
  lower = c(-Inf, -Inf, 0, 0, 0),
  upper = c(Inf, Inf, Inf, Inf, 1),
  add_details = FALSE
)
#> $estimate
#> [1] 2.0186087 4.2733443 0.2356257 0.4370632 0.3484053
#> 
#> $estimate_split
#> $estimate_split$mu
#> [1] 2.018609 4.273344
#> 
#> $estimate_split$sd
#> [1] 0.2356257 0.4370632
#> 
#> $estimate_split$lambda
#> [1] 0.3484053
#> 
#> 
#> $value
#> [1] -276.36
#> 
#> $seconds
#> [1] 0.6203501
#> 
#> $stopping_reason
#> [1] "change in function value between 1 iteration is < 1e-06"
#> 
```
