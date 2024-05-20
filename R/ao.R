#' Alternating Optimization
#'
#' @description
#' Performing alternating optimization of the function \code{f}.
#'
#' - `ao` is the most general function
#' - `ao_fixed` is the special case of a fixed partition
#' - `ao_random` is the special case of random partitions
#'
#' @param objective (`numeric()`)\cr
#' The definition of the objective function to be optimized via alternating
#' optimization, can be created via \code{\link[optimizeR]{Objective}}.
#'
#' @param partition (`numeric()`)\cr
#' The definition of the target argument partition for alternating optimization,
#' can be created via \code{\link{Partition}}.
#'
#' @param optimizer (`Optimizer`)\cr
#' The definition of the base optimizer that solves the optimization problems in
#' the partitions, can be created via \code{\link[optimizeR]{Optimizer}}.
#'
#' @param initial (`numeric()`)\cr
#' The starting parameter values for the optimization.
#'
#' @param minimize (`logical(1)`)\cr
#' Perform minimization? Alternatively, maximization is performed.
#'
#' @param iterations (`integer(1)`)\cr
#' The maximum number of iterations through the parameter partition before the
#' alternating optimization process is terminated. Can also be \code{Inf}, in
#' which case \code{tolerance} is responsible for the termination.
#'
#' @param tolerance (`numeric(1)`)\cr
#' A non-negative tolerance value. The alternating optimization terminates
#' prematurely (i.e., before \code{iterations} is reached) if the euclidean
#' distance between the current estimate and the one from the last iteration is
#' smaller than \code{tolerance}.
#'
#' @param joint_end (`logical(1)`)\cr
#' Optimize the parameter set jointly after the alternating optimization process
#' is terminated?
#'
#' @param verbose (`logical(1)`)\cr
#' Print tracing details during the alternating optimization process?
#'
#' @param f (`function`)\cr
#' A \code{function} to be optimized, returning a single \code{numeric}.
#' The first argument of \code{f} must be a \code{numeric} of the same length as
#' \code{initial} followed by any other arguments specified by the \code{...}
#' argument.
#'
#' @param ...
#' Additional arguments to be passed to \code{f}.
#'
#' @param fixed_partition (`list()`)\cr
#' A \code{list} of vectors of indices of \code{initial}, specifying the
#' partition of the parameter vector in the alternating optimization process.
#' The default is \code{as.list(1:length(initial))}, i.e. each parameter is
#' optimized separately. Parameter indices can be members of multiple blocks.
#'
#' @param new_block_probability (`numeric(1)`)\cr
#' The probability for a new parameter block in random partitions.
#' Values close to 0 result in larger parameter blocks, values close to 1
#' result in smaller parameter blocks.
#'
#' @param minimum_block_number (`integer(1)`)\cr
#' The minimum number of blocks in random partitions.
#'
#' @return
#' A \code{list} with the elements
#' * \code{estimate}, the optimal parameter vector found,
#' * \code{value}, the value of \code{f} at \code{estimate},
#' * \code{sequence}, a \code{data.frame} of the function values, estimates and
#'   computation times in the single iterations and parameter blocks,
#' * and \code{seconds}, the overall computation time in seconds.
#'
#' @examples
#' # definition of the 'Himmelblau' objective function with two parameters
#' objective <- Objective$new(
#'   f = function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2,
#'   npar = 2
#' )
#'
#' # definition of the partition: one parameter conditional on the other
#' partition <- Partition$new(npar = 2, type = "sequential")
#'
#' # definition of the optimizer with parameter restriction: -5 <= x_1, x_2 <= 5
#' optimizer <- Optimizer$new(
#'   which = "stats::optim", lower = -5, upper = 5, method = "L-BFGS-B"
#' )
#'
#' # alternating optimization
#' ao(
#'   objective = objective,
#'   partition = partition,
#'   optimizer = optimizer,
#'   initial = c(0, 0),     # initial parameter values
#'   minimize = TRUE,       # minimization
#'   iterations = Inf,      # no restriction on the number of iterations
#'   tolerance = 1e-6,      # stop if change in parameters is within tolerance
#'   joint_end = TRUE,      # finally perform joint optimization
#'   verbose = TRUE         # print progress
#' )
#'
#' @export

ao <- function(
  objective,
  partition = Partition$new(npar = sum(objective$npar), type = "sequential"),
  optimizer = Optimizer$new("stats::optim"),
  initial = stats::rnorm(sum(objective$npar)),
  minimize = TRUE,
  iterations = 10,
  tolerance = 1e-6,
  joint_end = FALSE,
  verbose = TRUE
) {

  ### input checks
  if (!checkmate::test_class(objective, "Objective")) {
    cli::cli_abort(
      "{.var objective} must be an
      {.help [{.cls Objective}](optimizeR::Objective)} object",
      call = NULL
    )
  }
  if (!checkmate::test_class(partition, "Partition")) {
    cli::cli_abort(
      "{.var partition} must be an
      {.help [{.cls Partition}](ao::Partition)} object",
      call = NULL
    )
  }
  if (sum(objective$npar) != partition$npar) {
    cli::cli_abort(
      "parameter number implied by {.var objective}
      ({.num {sum(objective$npar)}}) does not match parameter number
      implied by {.var partition} ({.num {partition$npar}})",
      call = NULL
    )
  }
  npar <- partition$npar
  if (!checkmate::test_class(optimizer, "Optimizer")) {
    cli::cli_abort(
      "{.var optimizer} must be an
      {.help [{.cls Optimizer}](optimizeR::Optimizer)} object",
      call = NULL
    )
  }
  if (!oeli::test_numeric_vector(initial, len = npar)) {
    cli::cli_abort(
      "{.var initial} must be a vector of initial values of length
      {.num {npar}}",
      call = NULL
    )
  }
  if (!checkmate::test_flag(minimize)) {
    cli::cli_abort(
      "{.var minimize} must be TRUE or FALSE",
      call = NULL
    )
  }
  if (!checkmate::test_number(iterations, lower = 1, finite = FALSE)) {
    cli::cli_abort(
      "{.var iterations} must be an integer greater or equal {.num 1}",
      call = NULL
    )
  } else if (is.finite(iterations)) {
    iterations <- as.integer(iterations)
  }
  if (!checkmate::test_number(tolerance, lower = 0, finite = TRUE)) {
    cli::cli_abort(
      "{.var tolerance} must be a single, non-negative number",
      call = NULL
    )
  } else if (tolerance == 0 && identical(iterations, Inf)) {
    cli::cli_abort(
      "{.var tolerance} cannot be {.num 0} if {.var iterations} is {.num Inf}",
      call = NULL
    )
  }
  if (!checkmate::test_flag(joint_end)) {
    cli::cli_abort(
      "{.var joint_end} must be TRUE or FALSE",
      call = NULL
    )
  }
  if (!checkmate::test_flag(verbose)) {
    cli::cli_abort(
      "{.var verbose} must be TRUE or FALSE",
      call = NULL
    )
  }

  ### prepare block objective function
  block_objective <- function(block) {
    function(theta_block, theta_rest) {
      theta <- numeric(npar)
      theta[block] <- theta_block
      theta[-block] <- theta_rest
      out <- objective$evaluate(theta)

      # if ("gradient" %in% names(attributes(out))) {
      #   gradient <- attr(out, "gradient")
      #   if (is.numeric(gradient) && is.vector(gradient)) {
      #     attr(out, "gradient") <- gradient[p_ind]
      #   }
      # }
      # if ("hessian" %in% names(attributes(out))) {
      #   hessian <- attr(out, "hessian")
      #   if (is.numeric(hessian) && is.matrix(hessian)) {
      #     attr(out, "hessian") <- hessian[p_ind, p_ind, drop = FALSE]
      #   }
      # }
      out
    }
  }

  ### prepare output
  f_initial <- objective$evaluate(initial)
  sequence <- structure(
    data.frame(
      t(c(0L, f_initial, 0, initial, rep(NA, npar)))
    ),
    names = c(
      "iteration", "value", "seconds", paste0("p", seq_len(npar)),
      paste0("b", seq_len(npar))
    )
  )
  parameter_columns <- which(startsWith(colnames(sequence), "p"))

  ### start alternating optimization
  if (verbose) {
    cat("start alternating optimization \n")
  }
  est <- initial
  exit_flag <- FALSE
  iteration <- 1L
  while (iteration <= iterations) {

    if (exit_flag) {
      break
    }
    if (verbose) {
      cat("iteration", iteration, "of", iterations, "\n")
    }
    current_parameter <- unlist(sequence[nrow(sequence), parameter_columns])
    current_partition <- partition$get()

    ### optimize over each parameter block in current partition
    for (block in current_partition) {
      if (verbose) {
        cat("- block {", paste(block, sep = ","), "} : ")
      }
      block_objective_out <- optimizer$minimize(
        objective = block_objective(block),
        initial = est[block],
        theta_rest = est[-block]
      )
      # if (isTRUE(block_objective_out$error)) {
      #   cli::cli_abort(block_objective_out$error_message, call = NULL)
      # }

      ### update output
      est[block] <- block_objective_out[["parameter"]]
      value <- block_objective_out[["value"]]
      seconds <- block_objective_out[["seconds"]]
      sequence[nrow(sequence) + 1, ] <-
        c(iteration, value, seconds, est, seq_len(npar) %in% block)
      if (verbose) {
        cat("value =", value, "\n")
      }
    }

    ### check for break
    latest_parameter <- unlist(sequence[nrow(sequence), parameter_columns])
    dist <- sqrt(sum(current_parameter - latest_parameter)^2)
    if (dist < tolerance) {
      exit_flag <- TRUE
      if (verbose) {
        cat("tolerance reached : distance =", dist, "<", tolerance, "\n")
      }
      break
    } else {
      iteration <- iteration + 1
    }
  }

  ### joint end
  if (joint_end) {
    if (verbose) {
      cat("joint optimization in the end : ")
    }
    joint_objective_out <- optimizer$minimize(
      objective = objective,
      initial = est
    )
    est <- joint_objective_out[["parameter"]]
    value <- joint_objective_out[["value"]]
    seconds <- joint_objective_out[["seconds"]]
    sequence[nrow(sequence) + 1, ] <-
      c(NA_integer_, value, seconds, est, rep(1, npar))
    if (verbose) {
      cat("value =", value, "\n")
    }
  }

  ### return results
  if (verbose) {
    cat("finished alternating optimization \n")
  }
  list(
    "value" = value,
    "estimate" = est,
    "sequence" = sequence,
    "seconds" = sum(sequence$seconds)
  )
}

#' @rdname ao
#' @export

ao_fixed <- function(
    f,
    initial,
    ...,
    fixed_partition = as.list(1:length(initial)),
    minimize = TRUE,
    iterations = 10,
    tolerance = 1e-6,
    joint_end = FALSE,
    verbose = FALSE
) {
  objective <- Objective$new(f = f, npar = length(initial), ...)
  partition <- Partition$new(npar = length(initial), type = "fixed")$
    define_fixed_partition(fixed_partition = fixed_partition)
  optimizer <- Optimizer$new("stats::nlm")
  ao(
    objective = objective,
    partition = partition,
    optimizer = optimizer,
    initial = initial,
    minimize = minimize,
    iterations = iterations,
    tolerance = tolerance,
    joint_end = joint_end,
    verbose = verbose
  )
}

#' @rdname ao
#' @export

ao_random <- function(
    f,
    initial,
    ...,
    new_block_probability = 0.3,
    minimum_block_number = 1,
    minimize = TRUE,
    iterations = 10,
    tolerance = 1e-6,
    joint_end = FALSE,
    verbose = FALSE
) {
  objective <- Objective$new(objective = f, npar = length(initial), ...)
  partition <- Partition$new(npar = length(initial), type = "random")
  partition$new_block_probability <- new_block_probability
  partition$minimum_block_number <- minimum_block_number
  optimizer <- Optimizer$new("stats::nlm")
  ao(
    objective = objective,
    partition = partition,
    optimizer = optimizer,
    initial = initial,
    minimize = minimize,
    iterations = iterations,
    tolerance = tolerance,
    joint_end = joint_end,
    verbose = verbose
  )
}

