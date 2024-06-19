#' Alternating Optimization
#'
#' @description
#' Alternating optimization is an iterative procedure for optimizing a
#' real-valued function jointly over all its parameters by alternating
#' restricted optimization over parameter partitions.
#'
#' @param f (`function`)\cr
#' A \code{function} to be optimized, returning a single \code{numeric} value.
#'
#' The first argument of \code{f} should be a \code{numeric} of the same length
#' as \code{initial}, followed by any other arguments specified by the
#' \code{...} argument.
#'
#' If \code{f} is to be optimized over an argument other than the first, or more
#' than one argument, this has to be specified via the \code{target} argument.
#'
#' @param initial (`numeric()`)\cr
#' The starting parameter values for the target argument(s).
#'
#' @param target (`character()` or `NULL`)\cr
#' The name(s) of the argument(s) over which \code{f} gets optimized.
#'
#' This can only be \code{numeric} arguments.
#'
#' Can be `NULL` (default), then it is the first argument of `f`.
#'
#' @param npar (`integer()`)\cr
#' The length of the target argument(s).
#'
#' Must be specified if more than two target arguments are specified via
#' the `target` argument.
#'
#' Can be `NULL` if there is only one target argument, in which case `npar` is
#' set to be `length(initial)`.
#'
#' @param gradient (`function` or `NULL`)\cr
#' A \code{function} that returns the gradient of \code{f}.
#'
#' The function call of \code{gradient} must be identical to \code{f}.
#'
#' Can be `NULL`, in which case a finite-difference approximation will be used.
#'
#' @param ...
#' Additional arguments to be passed to \code{f} (and \code{gradient}).
#'
#' @param partition (`character(1)` or `list()`)\cr
#' Defines the parameter partition, and can be either
#' * `"sequential"` for treating each parameter separately,
#' * `"random"` for a random partition in each iteration,
#' * `"none"` for no partition (which is equivalent to joint optimization),
#' * or a `list` of vectors of parameter indices, specifying a custom
#'   partition for the alternating optimization process.
#'
#' @param new_block_probability (`numeric(1)`)\cr
#' Only relevant if `partition = "random"`.
#' The probability for a new parameter block when creating a random
#' partitions.
#' Values close to 0 result in larger parameter blocks, values close to 1
#' result in smaller parameter blocks.
#'
#' @param minimum_block_number (`integer(1)`)\cr
#' Only relevant if `partition = "random"`.
#' The minimum number of blocks in random partitions.
#'
#' @param minimize (`logical(1)`)\cr
#' Whether to minimize during the alternating optimization process.
#' If \code{FALSE}, maximization is performed.
#'
#' @param lower (`numeric()`)\cr
#' Optionally lower parameter bounds.
#'
#' @param upper (`numeric()`)\cr
#' Optionally upper parameter bounds.
#'
#' @param iteration_limit (`integer(1)` or `Inf`)\cr
#' The maximum number of iterations through the parameter partition before
#' the alternating optimization process is terminated.
#' Can also be `Inf` for no iteration limit.
#'
#' @param seconds_limit (`numeric(1)`)\cr
#' The time limit in seconds before the alternating optimization process is
#' terminated.
#' Can also be `Inf` for no time limit.
#' Note that this stopping criteria is only checked *after*
#' a sub-problem is solved and not *within* solving a sub-problem, so the
#' actual process time can exceed this limit.
#'
#' @param tolerance_value (`numeric(1)`)\cr
#' A non-negative tolerance value. The alternating optimization terminates
#' if the absolute difference between the current function value and the one
#' from the last iteration is smaller than \code{tolerance_value}.
#' Can be `0` for no value threshold.
#'
#' @param tolerance_parameter (`numeric(1)`)\cr
#' A non-negative tolerance value. The alternating optimization terminates
#' if the distance between the current estimate and the one from the last
#' iteration is smaller than \code{tolerance_parameter}.
#' Can be `0` for no parameter threshold.
#' By default, the distance is measured using the euclidean norm, but
#' another norm can be specified via the \code{tolerance_parameter_norm}
#' field.
#'
#' @param tolerance_parameter_norm (`function`)\cr
#' The norm that measures the distance between the current estimate and the
#' one from the last iteration. If the distance is smaller than
#' \code{tolerance_parameter}, the procedure is terminated.
#' It must be of the form \code{function(x, y)} for two vector inputs
#' \code{x} and \code{y}, and return a single \code{numeric} value.
#' By default, the euclidean norm \code{function(x, y) sqrt(sum((x - y)^2))}
#' is used.
#'
#' @param verbose (`logical(1)`)\cr
#' Whether to print tracing details during the alternating optimization
#' process.
#'
#' @return
#' A \code{list} with the following elements:
#' * \code{estimate} is the parameter vector at termination.
#' * \code{value} is the function value at termination.
#' * \code{details} is a `data.frame` with full information about the procedure:
#'   For each iteration (column `iteration`) it contains the function value
#'   (column `value`), parameter values (columns starting with `p` followed by
#'   the parameter index), the active parameter block (columns starting with `b`
#'   followed by the parameter index, where `1` stands for a parameter contained
#'   in the active parameter block and `0` if not), computation times in seconds
#'   (column `seconds`), and a code that summarizes whether the update got
#'   accepted (column `update_code`, where `0` stands for an accepted update,
#'   `1` for a rejected update due to an error when solving the sub-problem, and
#'   `2` for a rejected update because it did not improve the function value).
#' * \code{seconds} is the overall computation time in seconds.
#' * \code{stopping_reason} is a message why the procedure has terminated.
#'
#' @examples
#' # Example 1: Minimization of Himmelblau's function --------------------------
#'
#' himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
#' ao(f = himmelblau, initial = c(0, 0))
#'
#' # Example 2: Maximization of 2-class Gaussian mixture log-likelihood --------
#'
#' # target arguments:
#' # - class means mu (2, unrestricted)
#' # - class standard deviations sd (2, must be non-negative)
#' # - class proportion lambda (only 1 for identification, must be in [0, 1])
#'
#' normal_mixture_llk <- function(mu, sd, lambda, data) {
#'   c1 <- lambda * dnorm(data, mu[1], sd[1])
#'   c2 <- (1 - lambda) * dnorm(data, mu[2], sd[2])
#'   sum(log(c1 + c2))
#' }
#'
#' ao(
#'   f = normal_mixture_llk,
#'   initial = c(2, 4, 1, 1, 0.5),
#'   target = c("mu", "sd", "lambda"),
#'   npar = c(2, 2, 1),
#'   data = datasets::faithful$eruptions,
#'   partition = "random",
#'   minimize = FALSE,
#'   lower = c(-Inf, -Inf, 0, 0, 0),
#'   upper = c(Inf, Inf, Inf, Inf, 1)
#' )
#'
#' @export

ao <- function(
    f,
    initial,
    target = NULL,
    npar = NULL,
    gradient = NULL,
    ...,
    partition = "sequential",
    new_block_probability = 0.3,
    minimum_block_number = 2,
    minimize = TRUE,
    lower = -Inf,
    upper = Inf,
    iteration_limit = Inf,
    seconds_limit = Inf,
    tolerance_value = 1e-6,
    tolerance_parameter = 1e-6,
    tolerance_parameter_norm = function(x, y) sqrt(sum((x - y)^2)),
    verbose = FALSE) {
  ### input checks and building of objects
  ao_input_check(
    "initial",
    oeli::check_numeric_vector(initial, any.missing = FALSE)
  )
  if (is.null(npar)) {
    npar <- length(initial)
  }
  objective <- Objective$new(f = f, target = target, npar = npar, ...)
  npar <- sum(objective$npar)
  ao_input_check(
    "initial",
    oeli::check_numeric_vector(initial, len = npar)
  )
  ao_input_check(
    "lower",
    oeli::check_numeric_vector(lower, any.missing = FALSE)
  )
  if (length(lower) == 1) {
    lower <- rep(lower, npar)
  }
  ao_input_check(
    "lower",
    oeli::check_numeric_vector(lower, len = npar)
  )
  ao_input_check(
    "initial",
    if (any(initial < lower)) "Must respect lower limit {.var lower}" else TRUE
  )
  ao_input_check(
    "upper",
    oeli::check_numeric_vector(upper, any.missing = FALSE)
  )
  if (length(upper) == 1) {
    upper <- rep(upper, npar)
  }
  ao_input_check(
    "upper",
    oeli::check_numeric_vector(upper, len = npar)
  )
  ao_input_check(
    "initial",
    if (any(initial > upper)) "Must respect upper limit {.var upper}" else TRUE
  )
  if (!is.null(gradient)) {
    gradient <- Objective$new(f = gradient, target = target, npar = npar, ...)
  }
  procedure <- Procedure$new(
    npar = npar,
    partition = partition,
    new_block_probability = new_block_probability,
    minimum_block_number = minimum_block_number,
    verbose = verbose,
    minimize = minimize,
    iteration_limit = iteration_limit,
    seconds_limit = seconds_limit,
    tolerance_value = tolerance_value,
    tolerance_parameter = tolerance_parameter,
    tolerance_parameter_norm = tolerance_parameter_norm
  )
  optimizer <- Optimizer$new(
    which = "stats::optim",
    method = "L-BFGS-B",
    hessian = FALSE
  )

  ### build sub-problem template
  solve_sub_problem <- function(parameter_block, parameter_fixed, block) {
    ### build block objective function
    block_objective <- function(parameter_block) {
      theta <- numeric(npar)
      theta[block] <- parameter_block
      theta[-block] <- parameter_fixed
      objective$evaluate(theta)
    }

    ### build block gradient function
    block_gradient <- if (is.null(gradient)) {
      NULL
    } else {
      function(parameter_block, ...) {
        theta <- numeric(npar)
        theta[block] <- parameter_block
        theta[-block] <- parameter_fixed
        gradient$evaluate(theta)[block]
      }
    }

    ### set optimizer arguments
    optimizer$set_arguments(
      "gr" = block_gradient, "lower" = lower[block], "upper" = upper[block]
    )

    ### solve sub-problem
    optimizer$optimize(
      objective = block_objective,
      initial = parameter_block,
      direction = ifelse(procedure$minimize, "min", "max")
    )
  }

  ### start alternating optimization
  procedure$initialize_details(
    initial_parameter = initial,
    initial_value = objective$evaluate(initial)
  )
  while (TRUE) {
    ### check stopping criteria
    if (procedure$check_stopping()) {
      break
    } else {
      procedure$iteration <- procedure$iteration + 1L
    }

    ### optimize over each parameter block in partition
    for (block in procedure$get_partition()) {
      procedure$block <- block

      ### optimize block objective function
      sub_problem_out <- solve_sub_problem(
        parameter_block = procedure$get_parameter_latest("block"),
        parameter_fixed = procedure$get_parameter_latest("fixed"),
        block = block
      )

      ### check acceptance and update
      procedure$update_details(
        value = sub_problem_out[["value"]],
        parameter_block = sub_problem_out[["parameter"]],
        seconds = sub_problem_out[["seconds"]],
        error = sub_problem_out[["error"]]
      )
    }
  }

  ### return results
  procedure$output
}
