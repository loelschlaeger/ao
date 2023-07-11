test_that("ao input checks work", {
  f <- function(x) x
  expect_error(
    ao(),
    "'f' must be a function."
  )
  expect_error(
    ao(f = f),
    "'p' must be a numeric vector."
  )
  expect_error(
    ao(f = f, p = 1, partition = "a"),
    "'partition' must be a list of vectors of indices of 'p'."
  )
  expect_error(
    ao(f = f, p = 1, base_optimizer = "not_an_optimizer"),
    "Input 'base_optimizer' must be an object of class 'optimizer'."
  )
  expect_error(
    ao(f = f, p = 1, partition = list(1), iterations = "1"),
    "'iterations' must be a single number."
  )
  expect_error(
    ao(f = f, p = 1, partition = list(1), iterations = 1, tolerance = -1),
    "'tolerance' must be a single, non-negative numeric."
  )
  expect_error(
    ao(f = f, p = 1, partition = list(1), iterations = Inf, tolerance = 0),
    "'tolerance' cannot be 0 while 'iterations' is infinite."
  )
  expect_error(
    ao(f = f, p = 1, partition = list(1), f_partition = "bad"),
    "'f_partition' must be a list."
  )
  expect_error(
    ao(f = f, p = 1, partition = list(1), f_partition = list(NULL, NULL)),
    "'f_partition' must have the same length as 'partition'."
  )
  expect_error(
    ao(f = f, p = 1, joint_end = "bad"),
    "'joint_end' must be either TRUE or FALSE."
  )
  expect_error(
    ao(f = f, p = 1, verbose = "bad"),
    "'verbose' must be either TRUE or FALSE."
  )
  expect_error(
    ao(f = f, p = 1, plot = "bad"),
    "'plot' must be either TRUE or FALSE."
  )
})

test_that("ao works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  out <- ao(
    f = himmelblau, p = c(0,0), partition = list(1, 2),
    base_optimizer = optimizeR::optimizer_optim(
      lower = -5, upper = 5, method = "L-BFGS-B"
    )
  )
  expect_type(out, "list")
  expect_named(out, c("value", "estimate", "sequence", "seconds"))
  expect_length(out$estimate, 2)
})

test_that("ao with additional parameters works", {
  himmelblau <- function(x, a, b, c) {
    (x[1]^a + x[2] - b)^a + (x[1] + x[2]^a - c)^a
  }
  out <- ao(
    f = himmelblau, a = 2, b = 11, c = 7,
    p = c(0,0), partition = list(1, 2),
    base_optimizer = optimizeR::optimizer_optim(
      lower = -5, upper = 5, method = "L-BFGS-B"
    )
  )
  expect_type(out, "list")
  expect_named(out, c("value", "estimate", "sequence", "seconds"))
  expect_length(out$estimate, 2)
})

test_that("ao works with partition functions", {
  himmelblau <- function(x, ...) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  himmelblau_1 <- function(x_part, x_rest, ...) {
    himmelblau(c(x_part, x_rest))
  }
  himmelblau_2 <- function(x_part, theta_rest) {
    himmelblau(c(theta_rest, x_part))
  }
  expect_error(
    ao(
      f = himmelblau, p = c(0,0),
      f_partition = list(function() {}, himmelblau_2)
    ),
    "must have two arguments."
  )
  expect_error(
    ao(
      f = himmelblau, p = c(0,0), tmp = "tmp",
      f_partition = list(NULL, himmelblau_2)
    ),
    "must have two arguments and the ... argument."
  )
  expect_error(
    ao(
      f = himmelblau, p = c(0,0), tmp = "tmp",
      f_partition = list(himmelblau_1, NULL)
    ),
    "must have a second argumend named 'theta_rest'."
  )
  out <- ao(
    f = himmelblau, p = c(0,0), partition = list(1, 2),
    base_optimizer = optimizeR::optimizer_optim(
      lower = -5, upper = 5, method = "L-BFGS-B"
    ), f_partition = list(NULL, himmelblau_2)
  )
  expect_type(out, "list")
  expect_named(out, c("value", "estimate", "sequence", "seconds"))
  expect_length(out$estimate, 2)
})

test_that("NULL elements in partition are detected", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  expect_error(
    ao(
      f = himmelblau, p = c(0,0), partition = list(1, 2, NULL),
      base_optimizer = optimizeR::optimizer_optim(
        lower = -5, upper = 5, method = "L-BFGS-B"
      )
    ),
    "'partition' must be a list of vectors of indices of 'p'."
  )
})

test_that("gradient and Hessian work", {
  test_fun <- function(x) {
    structure(
      x^4 + 2*x - 5,
      "gradient" = 4*x^3 + 2,
      "hessian" = as.matrix(12*x^2)
    )
  }
  out <- ao(
    f = test_fun, p = -3, partition = list(1),
    base_optimizer = optimizeR::optimizer_nlm()
  )
  expect_type(out, "list")
})

test_that("printing progress works", {
  f <- function(x) (x+2)^2
  expect_output(
    ao(
      f = f, p = 0, base_optimizer = optimizeR::optimizer_nlm(), verbose = TRUE,
      joint_end = TRUE
    )
  )
})

test_that("plotting progress works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  pdf(file = NULL)
  out <- ao(
    f = himmelblau, p = c(0,0), partition = list(1, 2),
    base_optimizer = optimizeR::optimizer_optim(
      lower = -5, upper = 5, method = "L-BFGS-B"
    ),
    joint_end = TRUE, plot = TRUE
  )
  dev.off()
  expect_type(out, "list")
})
