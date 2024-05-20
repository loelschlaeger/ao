test_that("ao input checks work", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  objective <- Objective$new(f = himmelblau, target = "x", npar = 2)
  partition <- Partition$new(npar = 2, type = "sequential")
  optimizer <- optimizeR::Optimizer$new(
    which = "stats::optim", lower = -5, upper = 5, method = "L-BFGS-B"
  )
  expect_error(
    ao(objective = function() { }),
    "`objective` must be"
  )
  expect_error(
    ao(objective = objective, partition = list()),
    "`partition` must be"
  )
  expect_error(
    ao(
      objective = objective, partition = partition, optimizer = function() { }
    ),
    "`optimizer` must be"
  )
  expect_error(
    ao(
      objective = objective, partition = partition, optimizer = optimizer,
      initial = rnorm(3)
    ),
    "must be a vector of initial values of length 2"
  )
  expect_error(
    ao(
      objective = objective, partition = partition, optimizer = optimizer,
      initial = rnorm(2), iterations = "2"
    ),
    "`iterations` must be an integer greater or equal 1"
  )
  expect_error(
    ao(
      objective = objective, partition = partition, optimizer = optimizer,
      initial = rnorm(2), iterations = 2, tolerance = -1
    ),
    "`tolerance` must be a single, non-negative number"
  )
  expect_error(
    ao(
      objective = objective, partition = partition, optimizer = optimizer,
      initial = rnorm(2), iterations = Inf, tolerance = 0
    ),
    "`tolerance` cannot be 0 if `iterations` is Inf"
  )
  expect_error(
    ao(
      objective = objective, partition = partition, optimizer = optimizer,
      initial = rnorm(2), iterations = 10, tolerance = 0, joint_end = "bad"
    ),
    "`joint_end` must be TRUE or FALSE"
  )
  expect_error(
    ao(
      objective = objective, partition = partition, optimizer = optimizer,
      initial = rnorm(2), iterations = 10, tolerance = 0, joint_end = FALSE,
      verbose = "bad"
    ),
    "`verbose` must be TRUE or FALSE"
  )
})

test_that("ao works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  objective <- Objective$new(f = himmelblau, target = "x", npar = 2)
  partition <- Partition$new(npar = 2, type = "random")
  partition$minimum_block_number <- 2
  optimizer <- optimizeR::Optimizer$new(
    which = "stats::optim", lower = -5, upper = 5, method = "L-BFGS-B"
  )
  out <- ao(
    objective = objective, optimizer = optimizer, verbose = FALSE
  )
  expect_type(out, "list")
  expect_named(out, c("value", "estimate", "sequence", "seconds"))
  expect_length(out$estimate, 2)
})

test_that("ao with additional parameters works", {
  himmelblau <- function(x, a, b, c) {
    (x[1]^a + x[2] - b)^a + (x[1] + x[2]^a - c)^a
  }
  objective <- Objective$new(
    f = himmelblau, target = "x", npar = 2, a = 2, b = 11, c = 7
  )
  partition <- Partition$new(npar = 2, type = "random")
  partition$minimum_block_number <- 2
  optimizer <- optimizeR::Optimizer$new(
    which = "stats::optim", lower = -5, upper = 5, method = "L-BFGS-B"
  )
  out <- ao(
    objective = objective, optimizer = optimizer, verbose = FALSE
  )
  expect_type(out, "list")
  expect_named(out, c("value", "estimate", "sequence", "seconds"))
  expect_length(out$estimate, 2)
})

test_that("ao with NULL values for fixed arguments works", {
  f <- function(x, a, b, ind) {
    if (is.null(ind)) {
      (x[1]^2 + x[2] + a)^2 + (x[1] + x[2]^2 + b)^2 + (x[3] - 1)^2
    }
  }
  checkmate::expect_list(
    ao::ao_fixed(
      f = f,
      initial = c(0, 0, 0),
      a = -11,
      b = -7,
      ind = NULL,
      fixed_partition = list(1, 2:3),
      iterations = 10,
      tolerance = 1e-06,
      verbose = FALSE
    )
  )
})
