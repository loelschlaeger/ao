#' Alternating Optimization
#'
#' @description
#' This function performs alternating optimization of the function \code{f}.
#'
#' @param f
#' A \code{function} to be optimized, returning a single \code{numeric}.
#' The first argument of \code{f} must be a \code{numeric} of the same length as
#' \code{p} followed by any other arguments specified by the \code{...}
#' argument.
#' @param p
#' A \code{numeric} vector, the starting parameter values for the optimization.
#' @param ...
#' Additional arguments to be passed to \code{f}.
#' @param partition
#' A \code{list} of vectors of indices of \code{p}, specifying the partition of
#' the parameter vector in the alternating optimization process.
#' The default is \code{as.list(1:length(p))}, i.e. each parameter is
#' optimized separately.
#' Parameter indices can be members of multiple subsets.
#' @param base_optimizer
#' An \code{Optimizer} object, which can be created via
#' \code{\link[optimizeR]{Optimizer}}.
#' It numerically solves the optimization problems in the partitions.
#' @param iterations
#' An \code{integer}, the maximum number of iterations through
#' \code{partitions} before the alternating optimization process is terminated.
#' Can also be \code{Inf}, in which case \code{tolerance} is responsible for the
#' termination.
#' The default is \code{10}.
#' @param tolerance
#' A non-negative \code{numeric}. The alternating optimization terminates
#' prematurely (i.e., before \code{interations} is reached) if the euclidean
#' distance between the current estimate and the one from the last iteration is
#' smaller than \code{tolerance}.
#' The default is \code{1e-6}.
#' @param f_partition
#' A \code{list} of the same length as \code{partition}.
#' The \code{i}-th element can be a \code{function} that computes the value of
#' \code{f} for the \code{i}-th parameter set defined in \code{partition}.
#' The \code{function} must be of the form
#' \code{function(theta_part, theta_rest, ...)}, where
#' - \code{theta_part} receives the parameter set for the current partition
#'   (this argument can be named differently),
#' - \code{theta_rest} receives the remaining parameters
#'   (this argument must be named \code{theta_rest}),
#' - \code{...} receives the additional arguments to \code{f}.
#' Alternatively, it can be \code{NULL}, in which case \code{f} is used.
#' @param joint_end
#' If \code{TRUE}, the parameter set is optimized jointly after the alternating
#' optimization process is terminated.
#' The default is \code{FALSE}.
#' @param verbose
#' If \code{TRUE}, full tracing details are printed during the alternating
#' optimization process.
#' The default is \code{FALSE}.
#'
#' @return
#' A \code{list} with the elements
#' * \code{estimate}, the optimal parameter vector found,
#' * \code{value}, the value of \code{f} at \code{estimate},
#' * \code{sequence}, a \code{data.frame} of the function values, estimates and
#'   computation times in the single iterations and partitions,
#' * and \code{seconds}, the overall computation time in seconds.
#'
#' @examples
#' # definition of the Himmelblau function
#' himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
#'
#' # alternating minimization separately for x_1 and x_2
#' # parameter restriction: -5 <= x_1, x_2 <= 5
#' ao(
#'   f = himmelblau, p = c(0, 0), partition = list(1, 2), iterations = Inf,
#'   base_optimizer = optimizeR::Optimizer$new(
#'     which = "stats::optim", lower = -5, upper = 5, method = "L-BFGS-B"
#'   )
#' )
#'
#' @export

# ao <- function(
#  partition = Partition$new(f, p, ..., type = "random"),
#  optimizer = Optimizer$new("stats::optim")
#  iterations = 10, tolerance = 1e-6, joint_end = FALSE, verbose = FALSE
# }

ao <- function(
    f, p, ..., partition = as.list(1:length(p)),
    base_optimizer = optimizeR::Optimizer$new("stats::optim"),
    iterations = 10, tolerance = 1e-6,
    f_partition = vector(mode = "list", length = length(partition)),
    joint_end = FALSE, verbose = FALSE) {
  ### input checks
  if (missing(f)) {
    cli::cli_abort(
      "Please specify {.var f}",
      call = NULL
    )
  } else if (!checkmate::test_function(f)) {
    cli::cli_abort(
      "{.var f} must be a function",
      call = NULL
    )
  }
  if (missing(p)) {
    cli::cli_abort(
      "Please specify {.var p}",
      call = NULL
    )
  } else if (!oeli::test_numeric_vector(p)) {
    cli::cli_abort(
      "{.var p} must be a vector of initial values",
      call = NULL
    )
  }
  if (!checkmate::test_list(partition)) {
    cli::cli_abort(
      "{.var partition} must be a {.cls list}",
      call = NULL
    )
  } else if (!setequal(unlist(partition), seq_along(p)) ||
    any(sapply(partition, length) == 0)) {
    cli::cli_abort(
      "{.var partition} must only contain vectors of indices of {.var p}",
      call = NULL
    )
  }
  if (!inherits(base_optimizer, "Optimizer")) {
    cli::cli_abort(
      c(
        "{.var base_optimizer} must be an {.cls Optimizer} object",
        "i" = "Use {.fun optimizeR::Optimizer} to create such an object"
      ),
      call = NULL
    )
  }
  if (!checkmate::test_number(iterations, lower = 1, finite = FALSE)) {
    cli::cli_abort(
      "{.var iterations} must be an integer greater or equal 1",
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
      "{.var tolerance} cannot be 0 if {.var iterations} is Inf",
      call = NULL
    )
  }
  if (!checkmate::test_list(f_partition)) {
    cli::cli_abort(
      "{.var f_partition} must be a {.cls list}",
      call = NULL
    )
  } else if (length(f_partition) != length(partition)) {
    cli::cli_abort(
      "{.var f_partition} must have the same length as {.var partition}",
      call. = FALSE
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

  ### preparing partitions
  npar <- length(p)
  for (part in seq_along(partition)) {
    if (!is.function(f_partition[[part]])) {
      f_partition[[part]] <- function(theta_part, theta_rest, ...) {
        p_ind <- partition[[part]]
        theta <- numeric(npar)
        theta[p_ind] <- theta_part
        theta[-p_ind] <- theta_rest
        out <- f(theta, ...)
        if ("gradient" %in% names(attributes(out))) {
          gradient <- attr(out, "gradient")
          if (is.numeric(gradient) && is.vector(gradient)) {
            attr(out, "gradient") <- gradient[p_ind]
          }
        }
        if ("hessian" %in% names(attributes(out))) {
          hessian <- attr(out, "hessian")
          if (is.numeric(hessian) && is.matrix(hessian)) {
            attr(out, "hessian") <- hessian[p_ind, p_ind, drop = FALSE]
          }
        }
        out
      }
    } else {
      add_arguments_exist <- length(list(...)) > 0
      if (length(formals(f_partition[[part]])) < 2 + add_arguments_exist) {
        cli::cli_abort(
          "{.var f_partition[[{part}]]} must have two arguments
          {ifelse(add_arguments_exist, 'and the ... argument', '')}",
          call = NULL
        )
      }
      if (!identical(names(formals(f_partition[[part]]))[2], "theta_rest")) {
        cli::cli_abort(
          "{.var f_partition[[{part}]]} must have a second argument named
          {.var theta_rest}",
          call = NULL
        )
      }
    }
  }

  ### alternating optimization
  exit_flag <- FALSE
  est <- p
  f_initial <- try(f(est, ...), silent = TRUE)
  if (!checkmate::test_number(f_initial, finite = TRUE)) {
    cli::cli_abort(
      "{.var f(p)} is not a single, finite number",
      call = NULL
    )
  }
  seq <- structure(
    data.frame(t(c(0, NA_integer_, f_initial, 0, est))),
    names = c("iteration", "partition", "value", "seconds", paste0("p", 1:npar))
  )
  iteration <- 1
  while (iteration <= iterations) {
    if (exit_flag) {
      break
    }
    if (verbose) {
      cat("iteration", iteration, "of", iterations, "\n")
    }
    for (part in seq_along(partition)) {
      if (verbose) {
        cat("- partition", part, "of", length(partition), ": ")
      }
      p_ind <- partition[[part]]
      f_part_out <- base_optimizer$minimize(
        objective = f_partition[[part]], initial = est[p_ind],
        theta_rest = est[-p_ind], ...
      )
      if (isTRUE(f_part_out$error)) {
        cli::cli_abort(
          f_part_out$error_message,
          call = NULL
        )
      }
      est[p_ind] <- f_part_out[["parameter"]]
      value <- f_part_out[["value"]]
      if (verbose) {
        cat("f =", value, "\n")
      }
      seq <- rbind(seq, c(iteration, part, value, f_part_out[["seconds"]], est))
      if (nrow(seq) > length(partition)) {
        curr <- as.numeric(seq[nrow(seq) - length(partition), -(1:4)])
        last <- as.numeric(seq[nrow(seq), -(1:4)])
        dist <- sqrt(sum(curr - last)^2)
        if (dist < tolerance) {
          exit_flag <- TRUE
          if (verbose) {
            cat("tolerance reached : distance =", dist, "<", tolerance, "\n")
          }
          break
        }
      }
    }
    iteration <- iteration + 1
  }
  if (joint_end) {
    if (verbose) {
      cat("joint optimization in the end : ")
    }
    f_joint_out <- base_optimizer$minimize(
      objective = f, initial = est, ...
    )
    est <- f_joint_out[["parameter"]]
    value <- f_joint_out[["value"]]
    seq <- rbind(
      seq, c(NA_integer_, NA_integer_, value, f_joint_out[["seconds"]], est)
    )
    if (verbose) {
      cat("f =", value, "\n")
    }
  }

  ### return results
  list(
    "value" = value,
    "estimate" = est,
    "sequence" = seq,
    "seconds" = sum(seq$seconds)
  )
}
