check_ao_output <- function(x, target = NULL, add_details = TRUE) {
  has_target <- !is.null(target)
  names <- c("estimate", "value", "seconds", "stopping_reason")
  if (add_details) {
    names <- oeli::insert_vector_entry(names, "details", 2)
  }
  if (has_target) {
    names <- oeli::insert_vector_entry(names, "estimate_split", 1)
  }
  checkmate::expect_list(x, len = length(names))
  checkmate::expect_names(names(x), identical.to = names)
  if (has_target) {
    checkmate::expect_list(x$estimate_split, len = 3)
    checkmate::expect_names(names(x$estimate_split), identical.to = target)
  }
}

test_that("ao works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  ao_out <- ao(f = himmelblau, initial = c(0, 0))
  check_ao_output(ao_out)
  expect_equal(ao_out$value, 0, tolerance = 1e-6)
  expect_equal(himmelblau(ao_out$estimate), ao_out$value)
  ao_out <- ao(f = himmelblau, initial = c(0, 0), add_details = FALSE)
  check_ao_output(ao_out, add_details = FALSE)
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
  check_ao_output(ao_out)
  expect_equal(ao_out$value, 0, tolerance = 1e-6)
  ao_random_out <- ao(
    f = himmelblau, initial = c(0, 0), partition = "random", gradient = gradient
  )
  check_ao_output(ao_random_out)
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
    # switch to nlm because stats::optim does not support Hessian
    base_optimizer = optimizeR::Optimizer$new("stats::nlm")
  )
  check_ao_output(ao_out)
})

test_that("ao with additional parameters works", {
  himmelblau <- function(x, a, b, c) {
    (x[1]^a + x[2] - b)^a + (x[1] + x[2]^a - c)^a
  }
  expect_error(
    ao(f = himmelblau, initial = c(0, 0)),
    "Function argument `a` is required but not specified yet."
  )
  ao_out <- ao(f = himmelblau, initial = c(0, 0), a = 2, b = 11, c = 7)
  check_ao_output(ao_out)
})

test_that("ao with targets works", {
  himmelblau <- function(x, a, b, c) {
    (x[1]^a + x[2] - b)^a + (x[1] + x[2]^a - c)^a
  }
  target <- c("x", "b", "c")
  ao_out <- ao(
    f = himmelblau, initial = 1:4, target = target, npar = c(2, 1, 1), a = 1
  )
  check_ao_output(ao_out, target = target)
})

test_that("ao with NULL values for fixed arguments works", {
  f <- function(x, a, b, ind) {
    if (is.null(ind)) {
      (x[1]^2 + x[2] + a)^2 + (x[1] + x[2]^2 + b)^2 + (x[3] - 1)^2
    }
  }
  ao_out <- ao(f = f, initial = c(0, 0, 0), a = -11, b = -7, ind = NULL)
  check_ao_output(ao_out)
})

test_that("ao with a different base optimizer works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  ao_out <- ao(
    f = himmelblau, initial = c(0, 0),
    base_optimizer = optimizeR::Optimizer$new("stats::nlm")
  )
  check_ao_output(ao_out)
})

test_that("ao with custom partition works", {
  f <- function(x) (x[1]^2 + x[2])^2 + (x[1] + x[2]^2)^2 + (x[3] + x[4]^2)^2
  ao_custom <- ao(
    f = f, initial = c(1, 1, 1, 1), partition = list(1, 2, 3:4)
  )
  check_ao_output(ao_custom)
})

test_that("ao with a single parameter works", {
  rosenbrock <- function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
  f <- function(x) rosenbrock(c(x, 1))
  for (partition in c("sequential", "random", "none")) {
    ao_out <- ao(f = f, initial = 0.5, partition = partition)
    check_ao_output(ao_out)
    expect_equal(ao_out$estimate, 1, tolerance = 1e-3)
    expect_identical(colnames(ao_out$details)[3:4], c("p1", "b1"))
  }
  expect_error(
    ao(f = f, initial = 0.5, partition = "random", minimum_block_number = 2),
    "minimum_block_number"
  )
})

test_that("ao with maximization works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  f <- function(x) -himmelblau(x)
  ao_out <- ao(f = f, initial = c(0, 0), minimize = FALSE)
  check_ao_output(ao_out)
  expect_equal(ao_out$value, 0, tolerance = 1e-6)
  expect_identical(ao_out$value, max(ao_out$details$value))
  expect_equal(f(ao_out$estimate), ao_out$value)
  expect_equal(
    ao_out$estimate, ao(f = himmelblau, initial = c(0, 0))$estimate,
    tolerance = 1e-4
  )
})

test_that("ao with parameter bounds works", {
  rosenbrock <- function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
  lower <- 1.5
  upper <- 2.5
  ao_bounds <- ao(
    f = rosenbrock, initial = c(2, 2), lower = lower, upper = upper,
    iteration_limit = 10
  )
  check_ao_output(ao_bounds)
  expect_true(all(ao_bounds$estimate <= upper))
  expect_true(all(ao_bounds$estimate >= lower))
  expect_lt(ao_bounds$value, rosenbrock(c(2, 2)))
  f <- function(x) sum(x^2)
  expect_equal(ao(f = f, initial = c(2, 2, 2), lower = 1)$estimate, c(1, 1, 1))
  expect_equal(ao(f = f, initial = c(-2, -2), upper = -1)$estimate, c(-1, -1))
  expect_equal(
    ao(f = f, initial = c(2, 2), lower = c(-Inf, 1))$estimate, c(0, 1),
    tolerance = 1e-4
  )
})

test_that("ao with initial values on the bounds works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  gradient <- function(x) {
    c(
      4 * x[1] * (x[1]^2 + x[2] - 11) + 2 * (x[1] + x[2]^2 - 7),
      2 * (x[1]^2 + x[2] - 11) + 4 * x[2] * (x[1] + x[2]^2 - 7)
    )
  }
  expect_true(all(gradient(c(-5, -5)) < 0))
  ao_out <- ao(f = himmelblau, initial = c(-5, -5), lower = -5, upper = 5)
  expect_equal(ao_out$estimate, c(-3.779310, -3.283186), tolerance = 1e-4)
  expect_equal(ao_out$value, 0, tolerance = 1e-6)
  ao_out <- ao(f = himmelblau, initial = c(-5, 1), lower = -5, upper = 5)
  expect_gt(ao_out$estimate[1], -5)
  expect_equal(ao_out$value, 0, tolerance = 1e-6)
  ao_out <- ao(
    f = function(x) -himmelblau(x), initial = c(-5, -5), lower = -5, upper = 5,
    minimize = FALSE
  )
  expect_equal(ao_out$estimate, c(-3.779310, -3.283186), tolerance = 1e-4)
})

test_that("ao with a local optimum in a corner of the bounds works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  gradient <- function(x) {
    c(
      4 * x[1] * (x[1]^2 + x[2] - 11) + 2 * (x[1] + x[2]^2 - 7),
      2 * (x[1]^2 + x[2] - 11) + 4 * x[2] * (x[1] + x[2]^2 - 7)
    )
  }
  corner <- c(0, 0)
  expect_identical(gradient(corner), c(-14, -22))
  ao_out <- ao(f = himmelblau, initial = corner, lower = -5, upper = 0)
  expect_identical(ao_out$estimate, corner)
  expect_identical(ao_out$value, himmelblau(corner))
  expect_identical(max(ao_out$details$iteration), 1)
  expect_identical(
    ao_out$stopping_reason,
    "change in function value between 1 iteration is < 1e-06"
  )
  optim_out <- stats::optim(
    par = corner, fn = himmelblau, method = "L-BFGS-B", lower = -5, upper = 0
  )
  expect_identical(optim_out$par, corner)
  out_multi <- ao(
    f = himmelblau, initial = list(corner, c(-1, -1)), lower = -5, upper = 0
  )
  expect_equal(out_multi$estimate, c(-3.779310, -3.283186), tolerance = 1e-4)
  expect_equal(out_multi$values[[1]], himmelblau(corner))
})

test_that("ao rejects bad parameter bounds", {
  f <- function(x) sum(x^2)
  expect_error(ao(f = f, initial = 0, lower = 1), "lower limit")
  expect_error(ao(f = f, initial = 2, upper = 1), "upper limit")
  expect_error(
    ao(f = f, initial = c(0, 0), lower = 1, upper = -1),
    "Input `upper` is bad: Must not be smaller than the lower limit"
  )
  expect_error(
    ao(f = f, initial = c(0, 0), lower = c(-1, -1, -1)),
    "Input `lower` is bad: Must have length 2"
  )
  expect_error(
    ao(f = f, initial = c(0, 0), upper = 1:3),
    "Input `upper` is bad: Must have length 2"
  )
  expect_error(
    ao(f = f, initial = c(0, 0), upper = NA_real_),
    "Input `upper` is bad"
  )
  expect_error(
    ao(f = f, initial = c(0, 0), upper = "1"),
    "Input `upper` is bad"
  )
})

test_that("ao rejects inconsistent target and npar inputs", {
  f <- function(x, y) sum(x^2) + sum(y^2)
  expect_error(
    ao(f = f, initial = 1:2, target = c("x", "y")),
    "Input `npar` is bad: Must be specified"
  )
  expect_error(
    ao(f = f, initial = 1:2, target = c("x", "y"), npar = 2),
    "Input `npar` is bad: Must have length 2"
  )
  expect_error(
    ao(f = f, initial = 1:3, target = c("x", "y"), npar = c(1, 1)),
    "Input `initial` is bad: Must have length 2"
  )
  expect_error(
    ao(f = function(x) sum(x^2), initial = numeric()),
    "Input `initial` is bad: Must have length >= 1"
  )
})

test_that("ao rejects invalid custom partitions", {
  f <- function(x) sum(x^2)
  expect_error(ao(f = f, initial = 1:2, partition = list(1, 3)))
  expect_error(ao(f = f, initial = 1:2, partition = list(1, NA_integer_)))
  expect_error(
    ao(f = f, initial = 1:3, partition = list(1:2, integer(), 3)),
    "`partition` are bad: Must have length >= 1"
  )
  expect_error(
    ao(f = f, initial = 1:3, partition = list(c(1, 1), 2:3)),
    "`partition` are bad: Contains duplicated values"
  )
  expect_error(
    ao(f = f, initial = 1:3, partition = list()),
    "Input `partition` is bad: Must have length >= 1"
  )
  expect_error(
    ao(f = f, initial = 1:3, partition = "sequentiell"),
    "Input `partition` is bad"
  )
  ao_out <- ao(f = f, initial = c(1, 1, 1), partition = list(1:2, 2))
  expect_equal(ao_out$estimate, c(0, 0, 1), tolerance = 1e-4)
})

test_that("ao rejects starting values outside bounds", {
  f <- function(x) sum(x^2)
  expect_error(ao(f = f, initial = 0, lower = 1))
  expect_error(ao(f = f, initial = 2, upper = 1))
})

test_that("input checks for ao work", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  expect_error(
    ao(f = himmelblau, initial = c(0, 0), hide_warnings = "yes"),
    "Input `hide_warnings` is bad"
  )
  expect_error(
    ao(f = himmelblau, initial = c(0, 0), base_optimizer = stats::optim),
    "Input `base_optimizer` is bad"
  )
  expect_error(ao(initial = c(0, 0)), "Input `f` is bad")
  expect_error(ao(f = himmelblau), "Input `initial` is bad")
  expect_error(
    ao(f = himmelblau, initial = list()),
    "Input `initial` is bad: Must have length >= 1"
  )
  expect_error(
    ao(f = himmelblau, initial = c(0, 0), base_optimizer = list()),
    "Input `base_optimizer` is bad: Must have length >= 1"
  )
})

test_that("ao rejects functions that do not return a single number", {
  expect_error(
    ao(f = function(x) x^2, initial = c(1, 2)),
    "Value of `f` at `initial` is bad: Must have length 1"
  )
  expect_error(
    ao(f = function(x) "one", initial = c(1, 2)),
    "Value of `f` at `initial` is bad"
  )
  expect_error(
    ao(f = function(x) NULL, initial = c(1, 2)),
    "Value of `f` at `initial` is bad"
  )
  expect_error(
    ao(f = function(x) stop("no value"), initial = c(1, 2)),
    "no value"
  )
})

test_that("ao terminates if solving a sub-problem fails", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  f <- function(x) {
    if (x[1] > 1) stop("first parameter is too large")
    himmelblau(x)
  }
  ao_out <- ao(f = f, initial = c(0, 0))
  check_ao_output(ao_out)
  expect_match(
    ao_out$stopping_reason,
    "^solving for block \\[ 1 \\] failed: .*first parameter is too large$"
  )
  expect_length(gregexpr("threw an error", ao_out$stopping_reason)[[1]], 1)
  expect_identical(ao_out$details$iteration, c(0, 1, 1))
  expect_identical(ao_out$details$p1, c(0, 0, 0))
  expect_identical(ao_out$details$value[1:2], rep(himmelblau(c(0, 0)), 2))
  expect_lt(ao_out$value, himmelblau(c(0, 0)))
  expect_identical(ao_out$value, ao_out$details$value[3])
  expect_identical(ao_out$estimate, c(0, ao_out$details$p2[3]))
  f <- function(x) if (all(x == 0)) NA_real_ else himmelblau(x)
  ao_out <- ao(f = f, initial = c(0, 0))
  expect_match(ao_out$stopping_reason, "^solving for block \\[ 1 \\] failed: ")
  expect_identical(ao_out$details$iteration, c(0, 1, 1))
  expect_identical(ao_out$estimate, c(0, 0))
  expect_identical(ao_out$value, NA_real_)
})

test_that("repeated prefix of error message is removed", {
  prefix <- "Function evaluation threw an error: "
  stopping_reason <- function(error_message) {
    process <- Process$new(npar = 1)
    process$initialize_details(initial_parameter = 0, initial_value = 0)
    process$block <- 1
    process$update_details(
      value = NA, parameter_block = NA, seconds = 0, error = TRUE,
      error_message = error_message
    )
    process$check_stopping()
    process$output$stopping_reason
  }
  expect_identical(
    stopping_reason(paste0(prefix, prefix, prefix, "message")),
    paste0("solving for block [ 1 ] failed: ", prefix, "message")
  )
  expect_identical(
    stopping_reason(paste0(prefix, prefix, "message")),
    paste0("solving for block [ 1 ] failed: ", prefix, "message")
  )
  expect_identical(
    stopping_reason(paste0(prefix, "message")),
    paste0("solving for block [ 1 ] failed: ", prefix, "message")
  )
  expect_identical(
    stopping_reason("message"),
    "solving for block [ 1 ] failed: message"
  )
})

test_that("warnings can be hidden", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  f <- function(x) {
    warning("noisy")
    himmelblau(x)
  }
  base_optimizer <- optimizeR::Optimizer$new(
    which = "stats::optim", method = "L-BFGS-B"
  )
  expect_false(base_optimizer$hide_warnings)
  expect_silent(
    ao(
      f = f, initial = c(0, 0), iteration_limit = 1,
      base_optimizer = base_optimizer
    )
  )
  expect_false(base_optimizer$hide_warnings)
  warnings <- character()
  withCallingHandlers(
    ao(
      f = f, initial = c(0, 0), iteration_limit = 1,
      base_optimizer = base_optimizer, hide_warnings = FALSE
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true("noisy" %in% warnings)
  expect_false(base_optimizer$hide_warnings)
})

test_that("ao terminates as soon as a tolerance is reached", {
  booth <- function(x) (x[1] + 2 * x[2] - 7)^2 + (2 * x[1] + x[2] - 5)^2
  for (history in 1:2) {
    ao_out <- ao(f = booth, initial = c(0, 0), tolerance_history = history)
    expect_identical(
      ao_out$stopping_reason,
      paste(
        "change in function value between", history,
        ifelse(history > 1, "iterations", "iteration"), "is < 1e-06"
      )
    )
    details <- ao_out$details
    details <- details[!duplicated(details$iteration, fromLast = TRUE), ]
    later <- seq_len(nrow(details))[-seq_len(history)]
    value_change <- abs(details$value[later] - details$value[later - history])
    parameter_change <- sqrt(
      (details$p1[later] - details$p1[later - history])^2 +
        (details$p2[later] - details$p2[later - history])^2
    )
    tolerance_reached <- value_change < 1e-6 | parameter_change < 1e-6
    expect_identical(which(tolerance_reached), length(later))
  }
})

test_that("ao with custom norm for the parameter tolerance works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  inputs <- list()
  maximum_norm <- function(x, y) {
    inputs[[length(inputs) + 1]] <<- list(x, y)
    max(abs(x - y))
  }
  ao_out <- ao(
    f = himmelblau, initial = c(0, 0), tolerance_value = 0,
    tolerance_parameter = 1e-4, tolerance_parameter_norm = maximum_norm
  )
  expect_identical(
    ao_out$stopping_reason,
    "change in function parameters between 1 iteration is < 1e-04"
  )
  expect_true(
    all(
      vapply(
        unlist(inputs, recursive = FALSE),
        function(x) is.numeric(x) && is.null(dim(x)) && length(x) == 2,
        logical(1)
      )
    )
  )
  expect_identical(inputs[[1]][[2]], c(0, 0))
  details <- ao_out$details
  details <- details[!duplicated(details$iteration, fromLast = TRUE), ]
  last <- nrow(details)
  expect_identical(
    inputs[[length(inputs)]],
    list(
      c(details$p1[last], details$p2[last]),
      c(details$p1[last - 1], details$p2[last - 1])
    )
  )
  ao_out <- ao(
    f = himmelblau, initial = c(0, 0),
    tolerance_parameter_norm = function(x, y) sqrt(drop(crossprod(x - y)))
  )
  expect_equal(ao_out$value, 0, tolerance = 1e-6)
  expect_error(
    ao(
      f = himmelblau, initial = c(0, 0),
      tolerance_parameter_norm = function(a, b) max(abs(a - b))
    ),
    "Input `tolerance_parameter_norm` is bad"
  )
})

test_that("ao with iteration limit works", {
  rosenbrock <- function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
  ao_out <- ao(f = rosenbrock, initial = c(-1.2, 1), iteration_limit = 3)
  expect_identical(ao_out$stopping_reason, "iteration limit of 3 reached")
  expect_identical(ao_out$details$iteration, c(0, 1, 1, 2, 2, 3, 3))
  expect_error(
    ao(f = rosenbrock, initial = c(-1.2, 1), iteration_limit = 0),
    "Input `iteration_limit` is bad"
  )
})

test_that("ao with time limit works", {
  f <- function(x) {
    Sys.sleep(0.002)
    sum(x^2)
  }
  ao_out <- ao(f = f, initial = c(1, 1, 1), seconds_limit = 0.001)
  expect_identical(
    ao_out$stopping_reason, "time limit of 0.001 seconds reached"
  )
  expect_identical(ao_out$details$iteration, c(0, 1))
  expect_identical(ao_out$details$b1, c(0, 1))
  expect_equal(ao_out$estimate, c(0, 1, 1), tolerance = 1e-4)
  ao_out <- ao(f = f, initial = c(1, 1, 1), seconds_limit = 0)
  expect_identical(ao_out$stopping_reason, "time limit of 0 seconds reached")
  expect_identical(ao_out$estimate, c(1, 1, 1))
})

test_that("ao requires an active stopping criterion", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  expect_error(
    ao(
      f = himmelblau, initial = c(0, 0), tolerance_value = 0,
      tolerance_parameter = 0
    ),
    "Stopping criteria are bad"
  )
  expect_error(
    ao(
      f = himmelblau, initial = c(0, 0), tolerance_value = 0,
      tolerance_parameter = 0
    ),
    "At least one of the limits must be finite"
  )
  ao_out <- ao(
    f = himmelblau, initial = c(0, 0), tolerance_value = 0,
    tolerance_parameter = 0, iteration_limit = 2
  )
  check_ao_output(ao_out)
  expect_identical(ao_out$stopping_reason, "iteration limit of 2 reached")
  ao_out <- ao(
    f = himmelblau, initial = c(0, 0), tolerance_value = 0,
    tolerance_parameter = 0, seconds_limit = 0
  )
  check_ao_output(ao_out)
  expect_identical(ao_out$stopping_reason, "time limit of 0 seconds reached")
  ao_out <- ao(f = himmelblau, initial = c(0, 0), tolerance_parameter = 0)
  expect_identical(
    ao_out$stopping_reason,
    "change in function value between 1 iteration is < 1e-06"
  )
  ao_out <- ao(f = himmelblau, initial = c(0, 0), tolerance_value = 0)
  expect_identical(
    ao_out$stopping_reason,
    "change in function parameters between 1 iteration is < 1e-06"
  )
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
  check_ao_output(out_multi, add_details = FALSE)
})

test_that("multiple ao processes can omit process details", {
  f <- function(x) sum((x - 1)^2)
  out_multi <- ao(
    f = f,
    initial = list(c(0, 0), c(2, 2)),
    partition = "none",
    iteration_limit = 1,
    add_details = FALSE
  )
  check_ao_output(out_multi, add_details = FALSE)
  expect_false(
    any(
      c("details", "estimates", "values", "seconds_each") %in%
        names(out_multi)
    )
  )
})

test_that("ao fails graciously", {
  f <- function(x) x[1]^2 + x[2]^2
  expect_silent(ao_out <- ao(f = f, initial = 1)) # initial is mis-specified
  check_ao_output(ao_out)
  expect_identical(ao_out$estimate, 1)
  expect_identical(ao_out$value, NA_real_)
  expect_match(ao_out$stopping_reason, "^solving for block \\[ 1 \\] failed: ")
})

test_that("multiple ao processes with details work", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  initial <- list(c(0, 0), c(1, 1), c(-1, -1))
  set.seed(1)
  out_multi <- ao(f = himmelblau, initial = initial, partition = "random")
  expect_named(
    out_multi,
    c(
      "estimate", "estimates", "value", "values", "details", "seconds",
      "seconds_each", "stopping_reason", "stopping_reasons", "processes"
    )
  )
  optimal_process <- which.min(unlist(out_multi$values))
  expect_identical(out_multi$value, out_multi$values[[optimal_process]])
  expect_identical(out_multi$estimate, out_multi$estimates[[optimal_process]])
  expect_identical(
    out_multi$stopping_reason, out_multi$stopping_reasons[[optimal_process]]
  )
  expect_identical(sort(unique(out_multi$details$process)), 1:3)
  expect_identical(nrow(out_multi$processes), 3L)
  expect_equal(out_multi$seconds, sum(unlist(out_multi$seconds_each)))
  expect_length(out_multi$estimates, 3)
  for (process in seq_along(initial)) {
    details <- out_multi$details[out_multi$details$process == process, ]
    expect_identical(c(details$p1[1], details$p2[1]), initial[[process]])
    expect_equal(
      himmelblau(out_multi$estimates[[process]]), out_multi$values[[process]]
    )
  }
  set.seed(1)
  out_multi_2 <- ao(f = himmelblau, initial = initial, partition = "random")
  expect_identical(out_multi_2$estimates, out_multi$estimates)
  expect_identical(
    out_multi_2$details[, c("process", "b1", "b2")],
    out_multi$details[, c("process", "b1", "b2")]
  )
})

test_that("multiple ao processes are specified correctly", {
  f <- function(x) sum(x^2)
  out_multi <- ao(
    f = f,
    initial = list(c(0, 0), c(1, 1)),
    partition = list("sequential", "none"),
    base_optimizer = optimizeR::Optimizer$new("stats::nlm"),
    iteration_limit = 1
  )
  expect_identical(nrow(out_multi$processes), 4L)
  expect_identical(unique(out_multi$details$process), 1:4)
  expect_named(
    out_multi$processes, c("initial", "partition", "base_optimizer")
  )
  expect_identical(out_multi$processes$initial[[4]], c(1, 1))
  expect_identical(out_multi$processes$partition[[4]], "none")
  details <- out_multi$details[out_multi$details$process == 4, ]
  expect_identical(c(details$p1[1], details$p2[1]), c(1, 1))
  expect_identical(details$b1, c(0, 1))
  expect_identical(details$b2, c(0, 1))
})

test_that("multiple ao processes with custom Hessian work", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  gradient <- function(x) {
    c(
      4 * x[1] * (x[1]^2 + x[2] - 11) + 2 * (x[1] + x[2]^2 - 7),
      2 * (x[1]^2 + x[2] - 11) + 4 * x[2] * (x[1] + x[2]^2 - 7)
    )
  }
  hessian_calls <- 0
  hessian <- function(x) {
    hessian_calls <<- hessian_calls + 1
    matrix(
      c(
        12 * x[1]^2 + 4 * x[2] - 42, 4 * x[1] + 4 * x[2],
        4 * x[1] + 4 * x[2], 12 * x[2]^2 + 4 * x[1] - 26
      ),
      nrow = 2
    )
  }
  out_multi <- ao(
    f = himmelblau, initial = list(c(0, 0), c(1, 1)), gradient = gradient,
    hessian = hessian, base_optimizer = optimizeR::Optimizer$new("stats::nlm")
  )
  expect_equal(unlist(out_multi$values), c(0, 0), tolerance = 1e-6)
  expect_gt(hessian_calls, 0) # only counted if processes run sequentially
})

test_that("multiple ao processes without function value work", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  f <- function(x) if (x[1] < 0) NA_real_ else himmelblau(x)
  out_multi <- ao(f = f, initial = list(c(-1, 0), c(1, 1)))
  expect_identical(out_multi$values[[1]], NA_real_)
  expect_identical(out_multi$estimates[[1]], c(-1, 0))
  expect_match(out_multi$stopping_reasons[[1]], "failed")
  expect_identical(out_multi$value, out_multi$values[[2]])
  expect_equal(out_multi$value, 0, tolerance = 1e-6)
  out_multi <- ao(f = f, initial = list(c(-1, 0), c(-2, 0)))
  expect_identical(out_multi$value, NA_real_)
  expect_identical(out_multi$estimate, c(-1, 0))
})

test_that("multiple ao processes do not mask global variables", {
  f <- function(x) {
    x <- x + lower
    (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  }
  environment(f) <- globalenv()
  if (exists("lower", envir = globalenv(), inherits = FALSE)) {
    lower_old <- get("lower", envir = globalenv())
    on.exit(assign("lower", lower_old, envir = globalenv()), add = TRUE)
  } else {
    on.exit(rm("lower", envir = globalenv()), add = TRUE)
  }
  assign("lower", c(3, 2), envir = globalenv())
  out_multi <- ao(f = f, initial = list(c(1, 1), c(0.5, 0.5)))
  expect_equal(unlist(out_multi$values), c(0, 0), tolerance = 1e-6)
})

test_that("multiple ao processes report their progress", {
  skip_if_not_installed("progressr")
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  old_options <- options(progressr.enable = TRUE)
  on.exit(options(old_options), add = TRUE)
  types <- character()
  messages <- character()
  withCallingHandlers(
    ao(f = himmelblau, initial = list(c(0, 0), c(1, 1)), add_details = FALSE),
    progression = function(p) {
      types <<- c(types, p$type)
      messages <<- c(messages, conditionMessage(p))
    }
  )
  expect_identical(types[1], "initiate")
  expect_identical(types[length(types)], "finish")
  expect_identical(sum(types == "initiate"), 1L)
  expect_identical(sum(types == "finish"), 1L)
  expect_gte(sum(types == "update"), 2)
  expect_setequal(
    messages,
    c(
      "running 2 AO processes", "[process 1] started", "[process 1] finished",
      "[process 2] started", "[process 2] finished"
    )
  )
})

test_that("random partition works with ucminf optimizer", {
  skip_if_not_installed("ucminf")
  set.seed(1)

  # simple convex objective in 6D
  f <- function(x) sum((x - 1)^2)
  out <- ao(
    f = f,
    initial = rep(0, 6),
    partition = "random",
    new_block_probability = 0.35,
    minimum_block_number = 2,
    base_optimizer = optimizeR::Optimizer$new("ucminf::ucminf"),
    iteration_limit = 3, # keep it fast for CRAN
    add_details = TRUE
  )
  check_ao_output(out, add_details = TRUE)
  d <- out$details
  bcols <- grep("^b[0-9]+$", names(d), value = TRUE)
  expect_length(bcols, 6)

  # drop init row if present
  d2 <- d[d$iteration > 0, , drop = FALSE]

  # each step should activate at least one parameter and not exceed npar
  active_counts <- rowSums(as.matrix(d2[, bcols, drop = FALSE]) == 1)
  expect_true(all(active_counts >= 1))
  expect_true(all(active_counts <= 6))

  # block indicators must be binary (0/1)
  expect_true(all(unlist(d2[, bcols, drop = FALSE]) %in% c(0, 1)))
})
