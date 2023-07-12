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
#' A \code{list} of vectors of indices of \code{p}, specifying the partition in
#' the alternating optimization process.
#' The default is \code{as.list(1:length(p))}, i.e. each parameter is
#' optimized separately.
#' Parameter indices can be members of multiple subsets.
#' @param base_optimizer
#' An \code{optimizer} object, which can be specified via
#' \code{\link[optimizeR]{define_optimizer}}.
#' It numerically solves the optimization problems in the partitions.
#' The default optimizer is \code{\link[optimizeR]{optimizer_optim}}.
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
#' @param plot
#' If \code{TRUE}, parameter updates are plotted during the alternating
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
#' ### minimization of the Himmelblau function
#' ### alternating optimization separately for x_1 and x_2
#' ### parameter restriction: -5 <= x_1, x_2 <= 5
#' himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
#' ao(
#'   f = himmelblau, p = c(0, 0), partition = list(1, 2), iterations = Inf,
#'   base_optimizer = optimizer_optim(
#'     lower = -5, upper = 5, method = "L-BFGS-B"
#'   )
#' )
#'
#' @export
#'
#' @importFrom optimizeR optimizer_optim apply_optimizer
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_point scale_x_discrete theme_minimal labs

ao <- function(
    f, p, ..., partition = as.list(1:length(p)),
    base_optimizer = optimizeR::optimizer_optim(),
    iterations = 10, tolerance = 1e-6,
    f_partition = vector(mode = "list", length = length(partition)),
    joint_end = FALSE, verbose = FALSE, plot = FALSE
) {
  if (missing(f) || !is.function(f)) {
    ao_stop("'f' must be a function.")
  }
  if (missing(p) || !is.numeric(p)) {
    ao_stop("'p' must be a numeric vector.")
  }
  if (!is.list(partition) || any(!is_number(unlist(partition))) ||
      !setequal(unlist(partition), seq_along(p)) ||
      any(sapply(partition, length) == 0)) {
    ao_stop("'partition' must be a list of vectors of indices of 'p'.")
  }
  if (!inherits(base_optimizer, "optimizer")) {
    ao_stop(
      "Input 'base_optimizer' must be an object of class 'optimizer'.",
      "Use 'optimizeR::define_optimizer()' to create such an object."
    )
  }
  if (length(iterations) != 1 ||
      !(is_number(iterations) || identical(iterations, Inf))) {
    ao_stop("'iterations' must be a single number.")
  }
  if (length(tolerance) != 1 || !is.numeric(tolerance) || tolerance < 0) {
    ao_stop("'tolerance' must be a single, non-negative numeric.")
  }
  if (tolerance == 0 && identical(iterations, Inf)) {
    ao_stop("'tolerance' cannot be 0 while 'iterations' is infinite.")
  }
  if (!is.list(f_partition)) {
    ao_stop("'f_partition' must be a list.")
  }
  if (length(f_partition) != length(partition)) {
    ao_stop("'f_partition' must have the same length as 'partition'.")
  }
  if (!isTRUE(joint_end) && !isFALSE(joint_end)) {
    ao_stop("'joint_end' must be either TRUE or FALSE.")
  }
  if (!isTRUE(verbose) && !isFALSE(verbose)) {
    ao_stop("'verbose' must be either TRUE or FALSE.")
  }
  if (!isTRUE(plot) && !isFALSE(plot)) {
    ao_stop("'plot' must be either TRUE or FALSE.")
  }
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
        ao_stop(
          paste0(
            "'f_partition[[", part, "]]' must have two arguments",
            if (add_arguments_exist) " and the ... argument." else "."
          )
        )
      }
      if (!identical(names(formals(f_partition[[part]]))[2], "theta_rest")) {
        ao_stop(
          paste0(
            "'f_partition[[", part,
            "]]' must have a second argumend named 'theta_rest'."
          )
        )
      }
    }
  }
  exit_flag <- FALSE
  est <- p
  seq <- structure(
    data.frame(t(c(0, NA_integer_, f(est, ...), 0, est))),
    names = c("iteration", "partition", "value", "seconds", paste0("p", 1:npar))
  )
  if (plot) {
    data <- data.frame(x = 1:npar, y = est)
    vis <- ggplot2::ggplot(data, ggplot2::aes(.data$x, .data$y)) +
      ggplot2::geom_point() +
      ggplot2::scale_x_discrete(limits = factor(data$x)) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "Parameter index", y = "Parameter estimate", title = "")
  }
  plot_update <- function(est, title, subtitle) {
    vis$data$y <- est
    vis$labels$title <- title
    vis$labels$subtitle <- subtitle
    print(vis)
  }
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
      if (plot) {
        plot_update(
          est = est,
          title = paste("iteration", iteration, "of", iterations),
          subtitle = paste("partition", part, "of", length(partition))
        )
      }
      p_ind <- partition[[part]]
      f_part_out <- optimizeR::apply_optimizer(
        optimizer = base_optimizer, objective = f_partition[[part]],
        initial = est[p_ind], theta_rest = est[-p_ind], ...
      )
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
            cat("tolerance reached : distance =" , dist, "<", tolerance, "\n")
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
    f_joint_out <- optimizeR::apply_optimizer(
      optimizer = base_optimizer, objective = f, initial = est, ...
    )
    est <- f_joint_out[["parameter"]]
    value <- f_joint_out[["value"]]
    seq <- rbind(
      seq, c(NA_integer_, NA_integer_, value, f_joint_out[["seconds"]], est)
    )
    if (verbose) {
      cat("f =", value, "\n")
    }
    if (plot) {
      plot_update(
        est = est,
        title = "joint optimization in the end",
        subtitle = ""
      )
    }
  }
  list(
    "value" = value,
    "estimate" = est,
    "sequence" = seq,
    "seconds" = sum(seq$seconds)
  )
}

