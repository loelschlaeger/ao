#' Alternating optimization
#'
#' @description
#' This function performs alternating optimization on the function \code{f}.
#'
#' @param f
#' An object of class \code{ao_f}, i.e. the output of \code{\link{set_f}}.
#' @param partition
#' A list of vectors of parameter indices \eqn{1,...,n} of the function.
#' For example, choosing \code{partition = list(1, 2)} as
#' in the example optimizes each parameter separately, while choosing
#' \code{partition = list(1:2)} leads to joint optimization.
#' Parameter indices can be members of multiple subsets.
#' @param initial
#' A vector of length \code{f$npar} of initial parameter values.
#' Can also be a single numeric, which then is repeated \code{f$npar} times.
#' Per default, the algorithm is initialized at the origin.
#' @param iterations
#' The number of iterations through all subsets.
#' The default is \code{10}.
#' @param tolerance
#' A non-negative numeric value. The function terminates prematurely if the
#' euclidean distance between the current solution and the one from the last
#' iteration is smaller than \code{tolerance}.
#' Per default, \code{tolerance = 1e-6}.
#' @param minimize
#' If \code{TRUE} (the default), minimization, if \code{FALSE}, maximization.
#' @param progress
#' If \code{TRUE}, progress is printed. The default is \code{FALSE}.
#' @param plot
#' If \code{TRUE}, the parameter updates are plotted.
#'
#' @return
#' An object of class \code{ao}, which is a list of
#' \itemize{
#'   \item \code{optimum}, the optimal value,
#'   \item \code{estimate}, the parameter vector that yields the optimum,
#'   \item \code{sequence}, a \code{data.frame} of the estimates in the single
#'         iterations,
#'   \item \code{time}, the total estimation time in seconds.
#' }
#'
#' @examples
#' himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
#' f <- set_f(f = himmelblau, npar = 2, lower = -5, upper = 5)
#' ao(f = f, partition = list(1, 2))
#'
#' @export

ao <- function(f, partition, initial = 0, iterations = 10, tolerance = 1e-6,
               minimize = TRUE, progress = FALSE, plot = TRUE) {

  ### check inputs
  if (missing(f)) {
    stop("Please set 'f'.",
         call. = FALSE)
  }
  if (!inherits(f, "ao_f")) {
    stop("'f' must be of class 'ao_f'.",
         call. = FALSE)
  }
  if (missing(partition)) {
    stop("Please set 'partition'.",
         call. = FALSE)
  }
  if (!is.list(partition)) {
    stop("'partition' must be a list.",
         call. = FALSE)
  }
  if (any(!is_number(unlist(partition)))) {
    stop("'partition' must be a list of numbers.",
         call. = FALSE)
  }
  if (any(!unlist(partition) %in% seq_len(f$npar))) {
    stop("'partition' contains values that are not parameter indices.",
         call. = FALSE)
  }
  if (any(!seq_len(f$npar) %in% unlist(partition))) {
    warning(paste(
      "Parameter(s)",
      paste(setdiff(seq_len(f$npar), unlist(partition)), collapse = ", "),
      "do not get optimized."
    ), call. = FALSE)
  }
  if (!is.numeric(initial)) {
    stop("'initial' must be a numeric vector.",
         call. = FALSE)
  }
  if (length(initial) == 1) {
    initial <- rep(initial, f$npar)
  }
  if (length(initial) != f$npar) {
    stop("'initial' must be a numeric vector of length 'f$npar.'",
         call. = FALSE)
  }
  if (any(initial < f$lower) || any(initial > f$upper)) {
    stop("'initial' does not fulfill 'lower' and 'upper' constraints of 'f'.",
         call. = FALSE)
  }
  if (!(length(iterations) == 1 && is_number(iterations))) {
    stop("'iterations' must be a number.",
         call. = FALSE)
  }
  if (!(length(tolerance) == 1 && is.numeric(tolerance) && tolerance >= 0)) {
    stop("'tolerance' must be non-negative.",
         call. = FALSE)
  }
  if (!is.logical(minimize)) {
    stop("'minimize' must be a boolean.",
         call. = FALSE)
  }
  if (!is.logical(progress)) {
    stop("'progress' must be a boolean.",
         call. = FALSE)
  }

  ### setup
  exit_flag <- FALSE
  estimate <- initial
  sequence <- data.frame(t(c(0, 0, estimate)))
  colnames(sequence) <- c("iteration", "partition", paste0("p", 1:f$npar))
  t_start <- Sys.time()
  if (plot) {
    data <- data.frame(x = 1:f$npar, y = estimate)
    x <- y <- NULL
    vis <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      ggplot2::scale_x_discrete(limits = factor(data$x)) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "Parameter index", y = "", title = "")
  }

  for (i in seq_len(iterations)) {

    ### premature interruption
    if (exit_flag) {
      if (progress) {
        cat("premature interruption because 'tolerance' is reached\n")
      }
      break
    }

    ### print progress
    if (progress) {
      cat("iteration", i, "of", iterations, "\n")
    }

    for (p in seq_along(partition)) {

      ### print and plot progress
      if (progress) {
        cat("- partition", p, "of", length(partition), ":", f$f(estimate), "\n")
      }
      if (plot) {
        vis$data$y <- estimate
        vis$labels$title <- paste("iteration", i, "partition", p)
        print(vis)
      }

      ### indices of selected group
      p_ind <- partition[[p]]

      ### skip step if selected group is empty
      if (length(p_ind) == 0) {
        next
      }

      ### divide estimation problem
      divide <- function(theta_small) {
        theta <- numeric(f$npar)
        theta[p_ind] <- theta_small
        theta[-p_ind] <- estimate[-p_ind]
        out <- f$f(theta)
        if (!minimize) {
          out <- -out
        }
        if (!is.null(attr(out, "gradient", exact = TRUE))) {
          attr(out, "gradient") <- attr(out, "gradient",
                                        exact = TRUE)[p_ind]
        }
        if (!is.null(attr(out, "hessian", exact = TRUE))) {
          attr(out, "hessian") <- attr(out, "hessian",
                                       exact = TRUE)[p_ind, p_ind]
        }
        return(out)
      }

      ### (try to) solve divided estimation problem
      base_args <- list(divide, estimate[p_ind])
      names(base_args) <- f$optimizer$base_arg_names[1:2]
      conquer <- try_silent({
        do.call(
          what = f$optimizer$f,
          args = c(base_args, f$f_par, f$optimizer$args))
      })

      ### evaluate
      if (!inherits(conquer,"fail")) {
        estimate[p_ind] <- conquer[[f$optimizer$base_arg_names[4]]]
      }
      sequence <- rbind(sequence, c(i, p, estimate))

      ### check for premature interruption
      if (nrow(sequence) > length(partition)) {
        curr <- as.numeric(sequence[nrow(sequence) - length(partition), -(1:2)])
        last <- as.numeric(sequence[nrow(sequence), -(1:2)])
        if (euclidean(curr, last) < tolerance) {
          exit_flag <- TRUE
        }
      }
    }
  }

  ### end timer
  t_end <- Sys.time()

  ### compute optimal function value
  optimum <- f$f(estimate)

  ### prepare output
  structure(
    list(
      "optimum" = optimum,
      "estimate" = estimate,
      "sequence" = sequence,
      "time" = difftime(t_end, t_start, units = "secs")
    ),
    class = "ao"
  )
}

#' @exportS3Method
#' @noRd

print.ao <- function(x, ...) {
  cat("Optimum value:", zapsmall(x$optimum), "\n")
  cat("Optimum at:", zapsmall(x$estimate), "\n")
  cat("Optimization time:", signif(x$time, digits = 2), "seconds\n")
}
