set.seed(1)

# -----------------------------
# Simulate data (richer example)
# -----------------------------
n <- 200
x <- runif(n, -1, 1)
z <- runif(n, 0, 1)                 # variance covariate in [0,1]

# Mean model design
X <- cbind(1, x)
beta_true <- c(-1, 0.2)

# Variance model design: log(sigma^2) = Z %*% delta
Z <- cbind(
  1,              # intercept in variance
  z,
  z^2,
  sin(2*pi*z)
)
colnames(Z) <- c("1", "z", "z2", "sin2piz")

delta_true <- c(0.2, 2.0, -1.6, 1.0)

sigma2 <- as.vector(exp(Z %*% delta_true))
y <- as.vector(X %*% beta_true + rnorm(n, sd = sqrt(sigma2)))

# -----------------------------------------
# Helper: analytic beta given delta (WLS)
# -----------------------------------------
beta_given_delta <- function(delta, X, y, Z) {
  # w_i = 1 / sigma_i^2 = exp(-Z_i' delta)
  w <- as.vector(exp(-(Z %*% delta)))
  XtW <- t(X) * w
  solve(XtW %*% X, XtW %*% y)
}

# ------------------------------------------------------
# Objective: negative log-likelihood as function of delta
# Uses analytic beta(delta) internally
# ------------------------------------------------------
nll_delta <- function(delta, X, y, Z, beta) {
  mu <- as.vector(X %*% beta)
  sigma2 <- as.vector(exp(pmin(pmax(Z %*% delta, -20), 20)))
  0.5 * sum(log(sigma2) + (y - mu)^2 / sigma2)
}

# -----------------------------
# Alternating optimization loop
# -----------------------------
delta <- rep(0, ncol(Z))            # initial guess for variance params
beta  <- rep(0, ncol(X))            # initial guess for mean params

max_iter <- 500
tol <- 1e-8

cat("Columns of Z (variance model):\n")
print(colnames(Z))
cat("\n")

for (iter in 1:max_iter) {
  beta_old  <- beta
  delta_old <- delta


  # (2) Fix beta (implicitly through beta(delta) inside nll), update delta numerically
  # Use BFGS for multi-parameter delta
  opt <- optim(
    par = delta,
    fn  = nll_delta,
    X = X, y = y, Z = Z, beta = beta,
    method = "BFGS"
  )
  delta <- opt$par

  # (1) Fix delta, update beta analytically
  beta <- beta_given_delta(delta, X, y, Z)

  # Convergence diagnostics
  step_beta  <- max(abs(beta - beta_old))
  step_delta <- max(abs(delta - delta_old))
  step <- max(step_beta, step_delta)

  # Evaluate objective at updated delta (consistent with beta(delta))
  obj <- nll_delta(delta, X, y, Z, beta)

  cat(sprintf(
    "iter %2d: nll = %.6f | step = %.3e (beta %.3e, delta %.3e) | optim it=%d, conv=%d\n",
    iter, obj, step, step_beta, step_delta, opt$counts[1], opt$convergence
  ))
  cat(sprintf("         beta  = (% .6f, % .6f)\n", beta[1], beta[2]))
  cat(sprintf("         delta = (% .6f, % .6f, % .6f, % .6f)\n\n",
              delta[1], delta[2], delta[3], delta[4]))

  if (step < tol) break
}

cat("True parameters:\n")
cat(sprintf("beta_true  = (% .3f, % .3f)\n", beta_true[1], beta_true[2]))
cat(sprintf("delta_true = (% .3f, % .3f, % .3f, % .3f)\n\n",
            delta_true[1], delta_true[2], delta_true[3], delta_true[4]))

cat("Estimated parameters:\n")
cat(sprintf("beta_hat  = (% .3f, % .3f)\n", beta[1], beta[2]))
cat(sprintf("delta_hat = (% .3f, % .3f, % .3f, % .3f)\n",
            delta[1], delta[2], delta[3], delta[4]))
