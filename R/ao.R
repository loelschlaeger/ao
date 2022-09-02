#' Alternating optimization
#'
#' @description
#' This function carries out alternating optimization of the function \code{f}.
#'
#' @details
#' For more details see the help vignette:
#' \code{vignette("ao", package = "ao")}
#'
#' @param f
#' The function to be optimized, returning a single numeric value. The first
#' argument of \code{f} must be a numeric vector of the length of \code{p}
#' followed by any other arguments specified by the \code{...} argument.
#' @param p
#' Starting parameter values for the optimization.
#' @param ...
#' Additional arguments to be passed to \code{f}.
#' @param partition
#' A list of vectors of indices of \code{p}, specifying the partition of the
#' alternating optimization.
#' The default is \code{as.list(1:length(p))}, i.e. each parameter is
#' optimized separately.
#' Parameter indices can be members of multiple subsets.
#' @param optimizer
#' An object of class \code{optimizer}, which can be specified via
#' \code{\link[optimizeR]{set_optimizer}}.
#' The default optimizer is \code{\link[stats]{optim}}.
#' @param iterations
#' The number of iterations through the partitions.
#' The default is \code{10}.
#' @param tolerance
#' A non-negative numeric value. The function terminates prematurely if the
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
#' A list containing the following components:
#' * \code{estimate}, the optimal set of parameters found,
#' * \code{optimum}, the value of \code{f} corresponding to \code{estimate},
#' * \code{sequence}, a data frame of the estimates and computation times in the
#'   single iterations,
#' * and \code{time}, the overall computation time.
#'
#' @examples
#' ### alternating optimization separately for x_1 and x_2
#' ### parameter restriction: -5 <= x_1, x_2 <= 5
#' himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
#' ao(
#'   f = himmelblau, p = c(0,0), partition = list(1, 2),
#'   optimizer = set_optimizer_optim(lower = -5, upper = 5, method = "L-BFGS-B")
#' )
#'
#' @export
#'
#' @importFrom rlang .data

ao <- function(
    f, p, ..., partition = as.list(1:length(p)),
    optimizer = set_optimizer_optim(), iterations = 10L, tolerance = 1e-6,
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
    ao_stop("'optimizer' must be an object of class 'optimizer'.")
  }
  if (length(iterations) != 1 || !is_number(iterations)) {
    ao_stop("'iterations' must be a single number.")
  }
  if (length(tolerance) != 1 || !is.numeric(tolerance) || tolerance < 0) {
    ao_stop("'tolerance' must be a single, non-negative numeric.")
  }
  if (length(print.level) != 1 ||  !print.level %in% c(0,1,2)) {
    ao_stop("'print.level' must be in {0,1,2}.")
  }
  if (!isTRUE(plot) && !isFALSE(plot)) {
    ao_stop("'plot' must be a boolean.")
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
    vis <- ggplot2::ggplot(data, ggplot2::aes(rlang::.data$x, rlang::.data$y)) +
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
            "- partition ", part, " of ", length(partition), ": f = ", f(est)
            ), "\n"
        )
      }
      if (plot) {
        vis$data$y <- est
        vis$labels$title <- paste("iteration", it, "partition", part)
        print(vis)
      }
      p_ind <- partition[[part]]
      if (length(p_ind) == 0) {
        next
      }
      f_small <- function(theta_small) {
        theta <- numeric(npar)
        theta[p_ind] <- theta_small
        theta[-p_ind] <- est[-p_ind]
        out <- f(theta)
        if (inherits(out, "gradient")) {
          attr(out, "gradient") <- attr(out, "gradient")[p_ind]
        }
        if (inherits(out, "hessian")) {
          attr(out, "hessian") <- attr(out, "hessian")[p_ind, p_ind]
        }
        out
      }
      f_small_out <- optimizeR(
        optimizer = optimizer, f = f_small, p = est[p_ind]#, ...
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
    "optimum" = f(est),
    "estimate" = est,
    "sequence" = seq,
    "time" = difftime(t_end, t_start)
  )
}

