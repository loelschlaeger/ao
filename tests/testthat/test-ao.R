test_that("ao works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  gradient <- function(x) {
    c(
      4 * x[1] * (x[1]^2 + x[2] - 11) + 2 * (x[1] + x[2]^2 - 7),
      2 * (x[1]^2 + x[2] - 11) + 4 * x[2] * (x[1] + x[2]^2 - 7)
    )
  }
  checkmate::expect_list(
    ao(f = himmelblau, initial = c(0, 0), gradient = gradient),
    len = 5
  )
  checkmate::expect_list(
    ao(f = himmelblau, initial = c(0, 0), partition = "random", gradient = gradient),
    len = 5
  )
})

test_that("ao with additional parameters works", {
  himmelblau <- function(x, a, b, c) {
    (x[1]^a + x[2] - b)^a + (x[1] + x[2]^a - c)^a
  }
  expect_error(
    ao(f = himmelblau, initial = c(0, 0)),
    "Function evaluation threw an error: argument \"a\" is missing, with no default"
  )
  checkmate::expect_list(
    ao(f = himmelblau, initial = c(0, 0), a = 2, b = 11, c = 7),
    len = 5
  )
})

test_that("ao with NULL values for fixed arguments works", {
  f <- function(x, a, b, ind) {
    if (is.null(ind)) {
      (x[1]^2 + x[2] + a)^2 + (x[1] + x[2]^2 + b)^2 + (x[3] - 1)^2
    }
  }
  checkmate::expect_list(
    ao(f = f, initial = c(0, 0, 0), a = -11, b = -7, ind = NULL),
    len = 5
  )
})

test_that("ao with a different base optimizer works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  expect_warning(
    checkmate::expect_list(
      ao(f = himmelblau, initial = c(0, 0), base_optimizer = optimizeR::optimizer_nlm()),
      len = 5
    ),
    "Arguments `gradient`, `lower`, and `upper` are ignored"
  )
})

test_that("ao with random partition works", {
  f <- function(x) (x[1]^2 + x[2])^2 + (x[1] + x[2]^2)^2 + (x[3] + x[4]^2)^2
  checkmate::expect_list(
    ao(f = f, initial = c(1, 1, 1, 1), partition = "random", iteration_limit = 10),
    len = 5
  )
})

test_that("ao with custom partition works", {
  f <- function(x) (x[1]^2 + x[2])^2 + (x[1] + x[2]^2)^2 + (x[3] + x[4]^2)^2
  checkmate::expect_list(
    ao(f = f, initial = c(1, 1, 1, 1), partition = list(1, 2, 3:4)),
    len = 5
  )
})
