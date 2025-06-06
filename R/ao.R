#' Alternating Optimization
#'
#' @description
#' Alternating optimization (AO) is an iterative process for optimizing a
#' real-valued function jointly over all its parameters by alternating
#' restricted optimization over parameter partitions.
#'
#' @details
#' ## Multiple processes
#' AO can suffer from local optima. To increase the likelihood of reaching the
#' global optimum, you can specify:
#'
#' * multiple starting parameters
#' * multiple parameter partitions
#' * multiple base optimizers
#'
#' Use the `initial`, `partition`, and/or `base_optimizer` arguments to provide
#' a `list` of possible values for each parameter. Each combination of initial
#' values, parameter partitions, and base optimizers will create a separate AO
#' process.
#'
#' ### Output value
#' In the case of multiple processes, the output values refer to the optimal
#' (with respect to function value) AO processes.
#'
#' If `add_details = TRUE`, the following elements are added:
#'
#' * \code{estimates} is a \code{list} of optimal parameters in each process.
#' * \code{values} is a \code{list} of optimal function values in each process.
#' * \code{details} combines details of the single processes and has an
#'   additional column `process` with an index for the different processes.
#' * \code{seconds_each} gives the computation time in seconds for each process.
#' * \code{stopping_reasons} gives the termination message for each process.
#' * \code{processes} give details how the different processes were specified.
#'
#' ### Parallel computation
#' By default, processes run sequentially. However, since they are independent,
#' they can be parallelized. To enable parallel computation, use the
#' [`{future}` framework](https://future.futureverse.org/). For example, run the
#' following *before* the `ao()` call:
#' \preformatted{
#' future::plan(future::multisession, workers = 4)
#' }
#'
#' ### Progress updates
#' When using multiple processes, setting `verbose = TRUE` to print tracing
#' details during AO is not supported. However, you can still track the progress
#' using the [`{progressr}` framework](https://progressr.futureverse.org/).
#' For example, run the following *before* the `ao()` call:
#' \preformatted{
#' progressr::handlers(global = TRUE)
#' progressr::handlers(
#'   progressr::handler_progress(":percent :eta :message")
#' )
#' }
#'
#' @param f \[`function`\]\cr
#' A \code{function} to be optimized, returning a single \code{numeric} value.
#'
#' The first argument of \code{f} should be a \code{numeric} of the same length
#' as \code{initial}, optionally followed by any other arguments specified by
#' the \code{...} argument.
#'
#' If \code{f} is to be optimized over an argument other than the first, or more
#' than one argument, this has to be specified via the \code{target} argument.
#'
#' @param initial \[`numeric()` | `list()`\]\cr
#' The starting parameter values for the target argument(s).
#'
#' This can also be a `list` of multiple starting parameter values, see details.
#'
#' @param target \[`character()` | `NULL`\]\cr
#' The name(s) of the argument(s) over which \code{f} gets optimized.
#'
#' This can only be \code{numeric} arguments.
#'
#' Can be `NULL` (default), then it is the first argument of `f`.
#'
#' @param npar \[`integer()`\]\cr
#' The length(s) of the target argument(s).
#'
#' Must be specified if more than two target arguments are specified via
#' the `target` argument.
#'
#' Can be `NULL` if there is only one target argument, in which case `npar` is
#' set to be `length(initial)`.
#'
#' @param gradient \[`function` | `NULL`\]\cr
#' Optionally a \code{function} that returns the gradient of \code{f}.
#'
#' The function call of \code{gradient} must be identical to \code{f}.
#'
#' Ignored if `base_optimizer` does not support custom gradient.
#'
#' @param hessian \[`function` | `NULL`\]\cr
#' Optionally a \code{function} that returns the Hessian of \code{f}.
#'
#' The function call of \code{hessian} must be identical to \code{f}.
#'
#' Ignored if `base_optimizer` does not support custom Hessian.
#'
#' @param ...
#' Additional arguments to be passed to \code{f} (and \code{gradient}).
#'
#' @param partition \[`character(1)` | `list()`\]\cr
#' Defines the parameter partition, and can be either
#'
#' * `"sequential"` for treating each parameter separately,
#' * `"random"` for a random partition in each iteration,
#' * `"none"` for no partition (which is equivalent to joint optimization),
#' * or a `list` of vectors of parameter indices, specifying a custom
#'   partition for the AO process.
#'
#' This can also be a `list` of multiple partition definitions, see details.
#'
#' @param new_block_probability \[`numeric(1)`\]\cr
#' Only relevant if `partition = "random"`.
#'
#' The probability for a new parameter block when creating a random
#' partition.
#'
#' Values close to 0 result in larger parameter blocks, values close to 1
#' result in smaller parameter blocks.
#'
#' @param minimum_block_number \[`integer(1)`\]\cr
#' Only relevant if `partition = "random"`.
#'
#' The minimum number of blocks in random partitions.
#'
#' @param minimize \[`logical(1)`\]\cr
#' Minimize during the AO process?
#'
#' If \code{FALSE}, maximization is performed.
#'
#' @param lower,upper \[`numeric()` | `NULL`\]\cr
#' Optionally lower and upper parameter bounds.
#'
#' Ignored if `base_optimizer` does not support parameter bounds.
#'
#' @param iteration_limit \[`integer(1)` | `Inf`\]\cr
#' The maximum number of iterations through the parameter partition before
#' the AO process is terminated.
#'
#' Can also be `Inf` for no iteration limit.
#'
#' @param seconds_limit \[`numeric(1)`\]\cr
#' The time limit in seconds before the AO process is terminated.
#'
#' Can also be `Inf` for no time limit.
#'
#' Note that this stopping criteria is only checked *after* a sub-problem is
#' solved and not *within* solving a sub-problem, so the actual process time can
#' exceed this limit.
#'
#' @param tolerance_value \[`numeric(1)`\]\cr
#' A non-negative tolerance value. The AO process terminates
#' if the absolute difference between the current function value and the one
#' before \code{tolerance_history} iterations is smaller than
#' \code{tolerance_value}.
#'
#' Can be `0` for no value threshold.
#'
#' @param tolerance_parameter \[`numeric(1)`\]\cr
#' A non-negative tolerance value. The AO process terminates if
#' the distance between the current estimate and the before
#' \code{tolerance_history} iterations is smaller than
#' \code{tolerance_parameter}.
#'
#' Can be `0` for no parameter threshold.
#'
#' By default, the distance is measured using the euclidean norm, but another
#' norm can be specified via the \code{tolerance_parameter_norm} argument.
#'
#' @param tolerance_parameter_norm \[`function`\]\cr
#' The norm that measures the distance between the current estimate and the
#' one from the last iteration. If the distance is smaller than
#' \code{tolerance_parameter}, the AO process is terminated.
#'
#' It must be of the form \code{function(x, y)} for two vector inputs
#' \code{x} and \code{y}, and return a single \code{numeric} value.
#' By default, the euclidean norm \code{function(x, y) sqrt(sum((x - y)^2))}
#' is used.
#'
#' @param tolerance_history \[`integer(1)`\]\cr
#' The number of iterations to look back to determine whether
#' \code{tolerance_value} or \code{tolerance_parameter} has been reached.
#'
#' @param base_optimizer \[`Optimizer` | `list()`\]\cr
#' An \code{Optimizer} object, which can be created via
#' \code{\link[optimizeR]{Optimizer}}. It numerically solves the sub-problems.
#'
#' By default, the \code{\link[stats]{optim}} optimizer with
#' \code{method = "L-BFGS-B"} is used.
#'
#' This can also be a `list` of multiple base optimizers, see details.
#'
#' @param verbose \[`logical(1)`\]\cr
#' Print tracing details during the AO process?
#'
#' Not supported when using multiple processes, see details.
#'
#' @param hide_warnings \[`logical(1)`\]\cr
#' Hide warnings during the AO process?
#'
#' @param add_details \[`logical(1)`\]\cr
#' Add details about the AO process to the output?
#'
#' @return
#' A \code{list} with the following elements:
#'
#' * \code{estimate} is the parameter vector at termination.
#' * \code{value} is the function value at termination.
#' * \code{details} is a `data.frame` with information about the AO process:
#'   For each iteration (column `iteration`) it contains the function value
#'   (column `value`), parameter values (columns starting with `p` followed by
#'   the parameter index), the active parameter block (columns starting with `b`
#'   followed by the parameter index, where `1` stands for a parameter contained
#'   in the active parameter block and `0` if not), and computation times in
#'   seconds (column `seconds`). Only available if `add_details = TRUE`.
#' * \code{seconds} is the overall computation time in seconds.
#' * \code{stopping_reason} is a message why the AO process has terminated.
#'
#' In the case of multiple processes, the output changes slightly, see details.
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
#' set.seed(123)
#'
#' ao(
#'   f = normal_mixture_llk,
#'   initial = runif(5),
#'   target = c("mu", "sd", "lambda"),
#'   npar = c(2, 2, 1),
#'   data = datasets::faithful$eruptions,
#'   partition = list("sequential", "random", "none"),
#'   minimize = FALSE,
#'   lower = c(-Inf, -Inf, 0, 0, 0),
#'   upper = c(Inf, Inf, Inf, Inf, 1),
#'   add_details = FALSE
#' )
#'
#' @export

ao <- function(
    f,
    initial,
    target = NULL,
    npar = NULL,
    gradient = NULL,
    hessian = NULL,
    ...,
    partition = "sequential",
    new_block_probability = 0.3,
    minimum_block_number = 1,
    minimize = TRUE,
    lower = NULL,
    upper = NULL,
    iteration_limit = Inf,
    seconds_limit = Inf,
    tolerance_value = 1e-6,
    tolerance_parameter = 1e-6,
    tolerance_parameter_norm = function(x, y) sqrt(sum((x - y)^2)),
    tolerance_history = 1,
    base_optimizer = Optimizer$new("stats::optim", method = "L-BFGS-B"),
    verbose = FALSE,
    hide_warnings = TRUE,
    add_details = TRUE
) {

  ### check if required arguments are specified
  oeli::input_check_response(
    check = oeli::check_missing(f),
    var_name = "f"
  )
  oeli::input_check_response(
    check = oeli::check_missing(initial),
    var_name = "initial"
  )

  ### multiple processes?
  if (
    is.list(initial) ||
    (is.list(partition) && !checkmate::test_list(partition, "numeric")) ||
    is.list(base_optimizer)
  ) {
    ### build processes
    if (!is.list(initial)) {
      initial <- list(initial)
    }
    if (!is.list(partition) || checkmate::test_list(partition, "numeric")) {
      partition <- list(partition)
    }
    if (!is.list(base_optimizer)) {
      base_optimizer <- list(base_optimizer)
    }
    processes <- expand.grid(
      initial = initial, partition = partition, base_optimizer = base_optimizer
    )
    nprocesses <- nrow(processes)

    ### run processes
    if (isTRUE(verbose)) {
      verbose <- FALSE
      if (!isTRUE(hide_warnings)) {
        cli::cli_warn("Argument {.var verbose} is set to {.code FALSE}")
      }
    }
    progress_step <- progressr::progressor(steps = nprocesses)
    progress_step(
      paste("running", nprocesses, "AO processes"),
      amount = 0, class = "sticky"
    )
    results <- future.apply::future_lapply(
      seq_len(nprocesses),
      function(process) {
        progress_step(
          paste0("[process ", process, "] started"),
          amount = 0, class = "sticky"
        )
        out <- ao(
          f = f,
          initial = processes[process, "initial"][[1]],
          target = target,
          npar = npar,
          gradient = gradient,
          ...,
          partition = processes[process, "partition"][[1]],
          new_block_probability = new_block_probability,
          minimum_block_number = minimum_block_number,
          minimize = minimize,
          lower = lower,
          upper = upper,
          iteration_limit = iteration_limit,
          seconds_limit = seconds_limit,
          tolerance_value = tolerance_value,
          tolerance_parameter = tolerance_parameter,
          tolerance_parameter_norm = tolerance_parameter_norm,
          tolerance_history = tolerance_history,
          base_optimizer = processes[process, "base_optimizer"][[1]],
          verbose = verbose,
          hide_warnings = hide_warnings,
          add_details = add_details
        )
        progress_step(
          paste0("[process ", process, "] finished"),
          class = "sticky"
        )
        return(out)
      },
      future.seed = TRUE
    )

    ### combine results
    values <- vapply(results, `[[`, numeric(1), "value")
    stopping_reasons <- vapply(results, `[[`, character(1), "stopping_reason")
    optimal_process <- ifelse(isTRUE(minimize), which.min(values), which.max(values))
    seconds_each <- vapply(results, `[[`, numeric(1), "seconds")
    if (isTRUE(add_details)) {
      details_list <- list()
      for (process in seq_len(nprocesses)) {
        details_list[[process]] <- cbind(
          process = process,
          results[[process]][["details"]]
        )
      }
      return(
        list(
          "estimate" = lapply(results, `[[`, "estimate")[[optimal_process]],
          "estimates" = lapply(results, `[[`, "estimate"),
          "value" = values[optimal_process],
          "values" = as.list(values),
          "details" = do.call("rbind", details_list),
          "seconds" = sum(seconds_each),
          "seconds_each" = as.list(seconds_each),
          "stopping_reason" = stopping_reasons[optimal_process],
          "stopping_reasons" = as.list(stopping_reasons),
          "processes" = processes
        )
      )
    } else {
      return(
        list(
          "estimate" = lapply(results, `[[`, "estimate")[[optimal_process]],
          "value" = values[optimal_process],
          "seconds" = sum(seconds_each),
          "stopping_reason" = stopping_reasons[optimal_process]
        )
      )
    }
  }

  ### input checks and building of objects
  oeli::input_check_response(
    check = oeli::check_numeric_vector(initial, any.missing = FALSE),
    var_name = "initial"
  )
  if (is.null(npar)) {
    npar <- length(initial)
  }
  objective <- Objective$new(f = f, target = target, npar = npar, ...)
  if (!is.null(gradient)) {
    oeli::input_check_response(
      check = checkmate::check_function(gradient),
      var_name = "gradient"
    )
    objective$set_gradient(gradient = gradient, .verbose = FALSE)
  }
  if (!is.null(hessian)) {
    oeli::input_check_response(
      check = checkmate::check_function(hessian),
      var_name = "hessian"
    )
    objective$set_hessian(hessian = hessian, .verbose = FALSE)
  }
  npar <- sum(objective$npar)
  oeli::input_check_response(
    check = oeli::check_numeric_vector(initial, len = npar),
    var_name = "initial"
  )
  oeli::input_check_response(
    check = oeli::check_numeric_vector(lower, any.missing = FALSE, null.ok = TRUE),
    var_name = "lower"
  )
  if (!is.null(lower)) {
    if (length(lower) == 1) {
      lower <- rep(lower, npar)
    }
    oeli::input_check_response(
      check = oeli::check_numeric_vector(lower, len = npar),
      var_name = "lower"
    )
    oeli::input_check_response(
      check = if (any(initial < lower)) "Must respect lower limit" else TRUE,
      var_name = "initial"
    )
  }
  oeli::input_check_response(
    check = oeli::check_numeric_vector(upper, any.missing = FALSE, null.ok = TRUE),
    var_name = "upper"
  )
  if (!is.null(upper)) {
    if (length(upper) == 1) {
      upper <- rep(upper, npar)
    }
    oeli::input_check_response(
      check = oeli::check_numeric_vector(upper, len = npar),
      var_name = "upper"
    )
    oeli::input_check_response(
      check = if (any(initial > upper)) "Must respect upper limit" else TRUE,
      var_name = "initial"
    )
  }
  oeli::input_check_response(
    check = checkmate::check_class(base_optimizer, "Optimizer"),
    var_name = "base_optimizer"
  )
  base_optimizer$hide_warnings <- hide_warnings

  ### building AO process
  process <- Process$new(
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
    tolerance_parameter_norm = tolerance_parameter_norm,
    tolerance_history = tolerance_history,
    add_details = add_details
  )

  ### build sub-problem template
  solve_sub_problem <- function(parameter_block, parameter_fixed, block) {

    block_objective <- Objective$new(
      f = function(p) {
        theta <- numeric(npar)
        theta[block] <- p
        theta[-block] <- parameter_fixed
        objective$evaluate(theta)
      },
      target = "p",
      npar = length(block)
    )

    ### build block gradient and Hessian functions
    if (!is.null(gradient)) {
      block_objective$set_gradient(
        gradient = function(p) {
          theta <- numeric(npar)
          theta[block] <- p
          theta[-block] <- parameter_fixed
          objective$evaluate_gradient(theta)[block]
        },
        .verbose = FALSE
      )
    }
    if (!is.null(hessian)) {
      block_objective$set_hessian(
        hessian = function(p) {
          theta <- numeric(npar)
          theta[block] <- p
          theta[-block] <- parameter_fixed
          objective$evaluate_hessian(theta)[block, block, drop = FALSE]
        },
        .verbose = FALSE
      )
    }

    ### solve sub-problem
    base_optimizer$optimize(
      objective = block_objective,
      initial = parameter_block,
      lower = if (!is.null(lower)) lower[block] else NA,
      upper = if (!is.null(upper)) upper[block] else NA,
      direction = ifelse(process$minimize, "min", "max")
    )
  }

  ### start AO
  process$initialize_details(
    initial_parameter = initial,
    initial_value = objective$evaluate(initial)
  )
  while (TRUE) {
    ### check stopping criteria
    if (process$check_stopping()) {
      break
    } else {
      process$iteration <- process$iteration + 1L
    }

    ### optimize over each parameter block in partition
    for (block in process$get_partition()) {
      process$block <- block

      ### optimize block objective function
      sub_problem_out <- solve_sub_problem(
        parameter_block = process$get_parameter_latest("block"),
        parameter_fixed = process$get_parameter_latest("fixed"),
        block = block
      )

      ### update details
      process$update_details(
        value = sub_problem_out[["value"]],
        parameter_block = sub_problem_out[["parameter"]],
        seconds = sub_problem_out[["seconds"]],
        error = sub_problem_out[["error"]],
        error_message = sub_problem_out[["error_message"]]
      )

    }
  }

  ### return results
  process$output
}
