test_that("ao works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  ao_out <- ao(f = himmelblau, initial = c(0, 0))
  checkmate::expect_list(ao_out, len = 5)
})

test_that("ao with custom gradient works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  gradient <- function(x) {
    c(
      4 * x[1] * (x[1]^2 + x[2] - 11) + 2 * (x[1] + x[2]^2 - 7),
      2 * (x[1]^2 + x[2] - 11) + 4 * x[2] * (x[1] + x[2]^2 - 7)
    )
  }
  ao_out <- ao(f = himmelblau, initial = c(0, 0), gradient = gradient)
  checkmate::expect_list(ao_out, len = 5)
  ao_random_out <- ao(f = himmelblau, initial = c(0, 0), partition = "random", gradient = gradient)
  checkmate::expect_list(ao_random_out, len = 5)
})

test_that("ao with custom Hessian works", {
  f <- function(x) {
    # print("huhu, my name is f")
    x^2
  }
  g <- function(x) {
    # print("huhu, my name is g")
    2*x
  }
  h <- function(x) {
    # print("huhu, my name is h")
    matrix(2) # important to have matrix(2) instead of 2!
  }
  ao_out <- ao(
    f = f, initial = 10, gradient = g, hessian = h,
    base_optimizer = Optimizer$new("stats::nlm") # switch to nlm because stats::optim does not support Hessian
  )
  checkmate::expect_list(ao_out, len = 5)
})

test_that("ao with additional parameters works", {
  himmelblau <- function(x, a, b, c) {
    (x[1]^a + x[2] - b)^a + (x[1] + x[2]^a - c)^a
  }
  expect_error(
    ao(f = himmelblau, initial = c(0, 0)),
    "Function evaluation threw an error: argument \"a\" is missing, with no default"
  )
  ao_out <- ao(f = himmelblau, initial = c(0, 0), a = 2, b = 11, c = 7)
  checkmate::expect_list(ao_out, len = 5)
})

test_that("ao with NULL values for fixed arguments works", {
  f <- function(x, a, b, ind) {
    if (is.null(ind)) {
      (x[1]^2 + x[2] + a)^2 + (x[1] + x[2]^2 + b)^2 + (x[3] - 1)^2
    }
  }
  ao_out <- ao(f = f, initial = c(0, 0, 0), a = -11, b = -7, ind = NULL)
  checkmate::expect_list(ao_out, len = 5)
})

test_that("ao with a different base optimizer works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  ao_out <- ao(f = himmelblau, initial = c(0, 0), base_optimizer = optimizeR::Optimizer$new("stats::nlm"))
  checkmate::expect_list(ao_out, len = 5)
})

test_that("ao with custom partition works", {
  f <- function(x) (x[1]^2 + x[2])^2 + (x[1] + x[2]^2)^2 + (x[3] + x[4]^2)^2
  ao_custom <- ao(f = f, initial = c(1, 1, 1, 1), partition = list(1, 2, 3:4))
  checkmate::expect_list(ao_custom, len = 5)
})

test_that("ao with parameter bounds works", {
  rosenbrock <- function(x) (1 - x[1])^2 + (x[2] - x[1]^2)^2
  lower <- 1.5
  upper <- 2.5
  initial <- runif(2, min = lower, max = upper)
  ao_bounds <- ao(f = rosenbrock, initial = initial, lower = lower, upper = upper)
  checkmate::expect_list(ao_bounds, len = 5)
  expect_true(all(ao_bounds$estimate <= upper))
  expect_true(all(ao_bounds$estimate >= lower))
})

test_that("multiple ao processes work", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  out_multi <- ao(
    f = himmelblau,
    initial = list(c(0, 0), c(1, 1)),
    partition = list("random", list(2, 1), "none", "sequential"),
    base_optimizer = list(
      optimizeR::Optimizer$new("stats::nlm"),
      optimizeR::Optimizer$new("stats::optim")
    ),
    add_details = FALSE
  )
  checkmate::expect_list(out_multi, len = 4)
})

test_that("ao fails graciously", {
  f <- function(x) x[1]^2 + x[2]^2
  expect_silent(ao(f = f, initial = 1)) # initial is mis-specified
})
