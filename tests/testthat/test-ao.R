test_that("ao input checks work", {
  f <- function(x) x
  expect_error(
    ao(),
    "must be a function"
  )
  expect_error(
    ao(f = f),
    "must be a numeric vector"
  )
  expect_error(
    ao(f = f, p = 1, partition = "a"),
    "must be a list of vectors"
  )
  expect_error(
    ao(f = f, p = 1, base_optimizer = "not_an_optimizer"),
    "must be an object of class 'optimizer'"
  )
  expect_error(
    ao(f = f, p = 1, partition = list(1), iterations = "1"),
    "must be a single number"
  )
  expect_error(
    ao(f = f, p = 1, partition = list(1), iterations = 1, tolerance = -1),
    "must be a single, non-negative numeric"
  )
  expect_error(
    ao(f = f, p = 1, print.level = 3),
    "must be one of 0, 1, 2"
  )
  expect_error(
    ao(f = f, p = 1, plot = "TRUE"),
    "must be either TRUE or FALSE"
  )
})

test_that("ao works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  out <- ao(f = himmelblau, p = c(0,0), partition = list(1, 2),
            base_optimizer = optimizeR::optimizer_optim(
              lower = -5, upper = 5, method = "L-BFGS-B"
            )
  )
  expect_type(out, "list")
  expect_named(out, c("optimum", "estimate", "sequence", "time"))
  expect_length(out$estimate, 2)
})

test_that("ao with additional parameters works", {
  himmelblau <- function(x, a, b, c) {
    (x[1]^a + x[2] - b)^a + (x[1] + x[2]^a - c)^a
  }
  out <- ao(f = himmelblau, a = 2, b = 11, c = 7,
            p = c(0,0), partition = list(1, 2),
            base_optimizer = optimizeR::optimizer_optim(
              lower = -5, upper = 5, method = "L-BFGS-B"
            )
  )
  expect_type(out, "list")
  expect_named(out, c("optimum", "estimate", "sequence", "time"))
  expect_length(out$estimate, 2)
})

test_that("NULL elements in partition do not break code", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  out <- ao(f = himmelblau, p = c(0,0), partition = list(1, 2, NULL),
            base_optimizer = optimizeR::optimizer_optim(
              lower = -5, upper = 5, method = "L-BFGS-B"
            )
  )
  expect_type(out, "list")
})

test_that("gradient and Hessian work", {
  test_fun <- function(x) {
    structure(
      x^4 + 2*x - 5,
      "gradient" = 4*x^3 + 2,
      "hessian" = as.matrix(12*x^2)
    )
  }
  out <- ao(f = test_fun, p = -3, partition = list(1),
            base_optimizer = optimizeR::optimizer_nlm()
  )
  expect_type(out, "list")
})

test_that("printing progress works", {
  f <- function(x) (x+2)^2
  expect_output(
    ao(f = f, p = 0, base_optimizer = optimizeR::optimizer_nlm(), print.level = 1)
  )
  expect_output(
    ao(f = f, p = 0, base_optimizer = optimizeR::optimizer_nlm(), print.level = 2)
  )
})

test_that("plotting progress works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  pdf(file = NULL)
  out <- ao(f = himmelblau, p = c(0,0), partition = list(1, 2),
            base_optimizer = optimizeR::optimizer_optim(
              lower = -5, upper = 5, method = "L-BFGS-B"
            ), plot = TRUE
  )
  dev.off()
  expect_type(out, "list")
})

