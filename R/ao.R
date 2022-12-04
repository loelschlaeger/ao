#' Alternating Optimization
#'
#' @description
#' This function performs alternating optimization of the function \code{f}.
#'
#' @param f
#' A \code{function} to be optimized, returning a single \code{numeric}.
#' The first argument of \code{f} must be a \code{numeric} of the length of
#' \code{p} followed by any other arguments specified by the \code{...}
#' argument.
#' @param p
#' A \code{numeric} vector, the starting parameter values for the optimization.
#' @param ...
#' Additional arguments to be passed to \code{f}.
#' @param partition
#' A \code{list} of vectors of indices of \code{p}, specifying the partition of
#' the alternating optimization.
#' The default is \code{as.list(1:length(p))}, i.e. each parameter is
#' optimized separately.
#' Parameter indices can be members of multiple subsets.
#' @param optimizer
#' An \code{optimizer} object, which can be specified via
#' \code{\link[optimizeR]{set_optimizer}}.
#' The default optimizer is \code{\link[stats]{optim}}.
#' @param iterations
#' An \code{integer}, the number of iterations through the parameter indices in
#' \code{partitions}.
#' The default is \code{10}.
#' @param tolerance
#' A non-negative \code{numeric}. The function terminates prematurely if the
#' euclidean distance between the current solution and the one from the last
#' iteration is smaller than \code{tolerance}.
#' The default is \code{1e-6}.
#' @param print.level
#' This argument determines the level of printing which is done during the
#' optimization process.
#' Three values (analogue to \code{\link[stats]{nlm}}) can be specified:
#' * \code{0} (the default): no printing
#' * \code{1}: initial and final details are printed
#' * \code{2}: full tracing information is printed
#' @param plot
#' If \code{TRUE}, the parameter updates are plotted.
#' The default is \code{FALSE}.
#'
#' @return
#' A \code{list} containing the following components:
#' * \code{estimate}: a \code{numeric}, the optimal parameter vector found,
#' * \code{optimum}: a \code{numeric}, the value of \code{f} at \code{estimate},
#' * \code{sequence}: a \code{data.frame} of the estimates and computation times
#'   in the single iterations,
#' * and \code{time}, a \code{difftime} object, the overall computation time.
#'
#' @examples
#' ### alternating optimization separately for x_1 and x_2
#' ### parameter restriction: -5 <= x_1, x_2 <= 5
#' himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
#' ao(
#'   f = himmelblau, p = c(0,0), partition = list(1, 2), iterations = 10,
#'   optimizer = optimizer_optim(lower = -5, upper = 5, method = "L-BFGS-B")
#' )
#'
#' @export
#'
#' @importFrom rlang .data

ao <- function(
    f, p, ..., partition = as.list(1:length(p)),
    optimizer = optimizer_optim(), iterations = 10, tolerance = 1e-6,
    print.level = 0, plot = FALSE
) {
  if (missing(f) || !is.function(f)) {
    ao_stop("'f' must be a function.")
  }
  if (missing(p) || !is.numeric(p)) {
    ao_stop("'p' must be a numeric vector.")
  }
  if (!is.list(partition) || any(!is_number(unlist(partition))) ||
      !setequal(unlist(partition), seq_along(p))) {
    ao_stop("'partition' must be a list of vectors of indices of 'p'.")
  }
  if (!inherits(optimizer, "optimizer")) {
    ao_stop(
      "'optimizer' must be an object of class 'optimizer'.",
      "Use 'optimizeR::set_optimizer()' to create such an object."
    )
  }
  if (length(iterations) != 1 || !is_number(iterations)) {
    ao_stop("'iterations' must be a single number.")
  }
  if (length(tolerance) != 1 || !is.numeric(tolerance) || tolerance < 0) {
    ao_stop("'tolerance' must be a single, non-negative numeric.")
  }
  if (length(print.level) != 1 ||  !print.level %in% c(0,1,2)) {
    ao_stop("'print.level' must be one of 0, 1, 2.")
  }
  if (!isTRUE(plot) && !isFALSE(plot)) {
    ao_stop("'plot' must be either TRUE or FALSE.")
  }
  exit_flag <- FALSE
  est <- p
  npar <- length(p)
  seq <- structure(
    data.frame(t(c(0, 0, 0, est))),
    names = c("iteration", "partition", "time", paste0("p", 1:npar))
  )
  t_start <- Sys.time()
  if (plot) {
    data <- data.frame(x = 1:npar, y = est)
    vis <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      ggplot2::scale_x_discrete(limits = factor(data$x)) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "Parameter index", y = "", title = "")
  }
  for (it in seq_len(iterations)) {
    print <- (print.level == 2 || (print.level == 1 && it %in% c(1, iterations)))
    if (exit_flag) {
      break
    }
    if (print) {
      cat("iteration", it, "of", iterations, "\n")
    }
    for (part in seq_along(partition)) {
      if (print) {
        cat(
          paste0(
            "- partition ", part, " of ", length(partition), ": f = ", f(est, ...)
            ), "\n"
        )
      }
      if (plot) {
        vis$data$y <- est
        vis$labels$title <- paste("iteration", it, "of", iterations)
        vis$labels$subtitle <- paste("partition", part, "of", length(partition))
        print(vis)
      }
      p_ind <- partition[[part]]
      if (length(p_ind) == 0) {
        next
      }
      f_small <- function(theta_small, ...) {
        theta <- numeric(npar)
        theta[p_ind] <- theta_small
        theta[-p_ind] <- est[-p_ind]
        out <- f(theta, ...)
        if (inherits(out, "gradient")) {
          attr(out, "gradient") <- attr(out, "gradient")[p_ind]
        }
        if (inherits(out, "hessian")) {
          attr(out, "hessian") <- attr(out, "hessian")[p_ind, p_ind]
        }
        out
      }
      f_small_out <- optimizeR::apply_optimizer(
        optimizer = optimizer, f = f_small, p = est[p_ind], ...
      )
      est[p_ind] <- f_small_out[["z"]]
      seq <- rbind(seq, c(it, part, f_small_out$time, est))
      if (nrow(seq) > length(partition)) {
        curr <- as.numeric(seq[nrow(seq) - length(partition), -(1:3)])
        last <- as.numeric(seq[nrow(seq), -(1:3)])
        if (sqrt(sum(curr - last)^2) < tolerance) {
          exit_flag <- TRUE
        }
      }
    }
  }
  t_end <- Sys.time()
  list(
    "optimum" = f(est, ...),
    "estimate" = est,
    "sequence" = seq,
    "time" = difftime(t_end, t_start)
  )
}

