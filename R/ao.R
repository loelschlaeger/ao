#' Alternating Optimization.
#'
#' This function performs alternating optimization on the function \code{f}.
#'
#' @details
#' This function depends on \code{\link[optimx]{optimx}}.
#'
#' @param f
#' An object of class \code{ao_f}, i.e. the output of \code{\link{set_f}}.
#' @param partition
#' A list of vectors of parameter indices \eqn{1,...,n} of the function.
#' @param initial
#' A vector of length \code{f$npar} of initial parameter values.
#' @param iterations
#' The number of iterations.
#' @param minimize
#' If \code{TRUE}, minimization, if \code{FALSE}, maximization.
#' @param progress
#' If \code{TRUE}, progress is printed.
#'
#' @return
#' An object of class \code{ao}, which is a list of
#' \itemize{
#'   \item \code{optimum}, the optimal value,
#'   \item \code{estimate}, the parameter vector that yields the optimum,
#'   \item \code{sequence}, a data frame of the estimates in the single iterations,
#'   \item \code{time}, the total estimation time in seconds.
#' }
#'
#' @examples
#' valley <- function(x) {
#'   cons <- c(1.003344481605351, -3.344481605351171e-03)
#'   n <- length(x)
#'   f <- rep(0, n)
#'   j <- 3 * (1:(n/3))
#'   jm2 <- j - 2
#'   jm1 <- j - 1
#'   f[jm2] <- (cons[2]*x[jm2]^3 + cons[1]*x[jm2]) * exp(-(x[jm2]^2)/100) - 1
#'   f[jm1] <- 10 * (sin(x[jm2]) - x[jm1])
#'   f[j] <- 10 * (cos(x[jm2]) - x[j])
#'   sum(f*f)
#' }
#' f <- set_f(f = valley, npar = 9, lower = 0, upper = 10, check = FALSE)
#' ao(f = f, partition = list(1, 2, 3, 4, 5, 6, 7, 8, 9), iterations = 2)
#'
#' @export

ao <- function(f, partition, initial = rep(0, f$npar),
               iterations = 1, minimize = TRUE, progress = FALSE) {

  ### check inputs
  if (missing(f)) {
    stop("Please set 'f'.", call. = FALSE)
  }
  if (class(f) != "ao_f") {
    stop("'f' must be of class 'ao_f'.", call. = FALSE)
  }
  if (missing(partition)) {
    stop("Please set 'partition'.", call. = FALSE)
  }
  if (!is.list(partition)) {
    stop("'partition' must be a list.", call. = FALSE)
  }
  if (any(!is_number(unlist(partition)))) {
    stop("'partition' must be a list of numbers.", call. = FALSE)
  }
  if (any(!unlist(partition) %in% seq_len(f$npar))) {
    stop("'partition' contains values that are not parameter indices.", call. = FALSE)
  }
  if (any(!seq_len(f$npar) %in% unlist(partition))) {
    warning(paste(
      "Parameter(s)",
      paste(setdiff(seq_len(f$npar), unlist(partition)), collapse = ", "),
      "do not get optimized."
    ), call. = FALSE)
  }
  if (!is.numeric(initial)) {
    stop("'initial' must be a numeric vector.", call. = FALSE)
  }
  if (length(initial) != f$npar) {
    stop("'initial' must be a numeric vector of length 'f$npar.'", call. = FALSE)
  }
  if(any(initial < f$lower) || any(initial > f$upper)) {
    stop("'initial' does not fulfill 'lower' and 'upper' constraints of 'f'.", call. = FALSE)
  }
  if (!(length(iterations) == 1 && is_number(iterations))) {
    stop("'iterations' must be a number.", call. = FALSE)
  }
  if (!is.logical(minimize)) {
    stop("'minimize' must be a boolean.", call. = FALSE)
  }
  if (!is.logical(progress)) {
    stop("'progress' must be a boolean.", call. = FALSE)
  }

  ### setup
  estimate <- initial
  sequence <- data.frame(t(c(0, 0, estimate)))
  colnames(sequence) <- c("iteration", "partition", paste0("p", 1:f$npar))
  t_start <- Sys.time()

  for (i in seq_len(iterations)) {

    ### print progress
    if (progress) {
      cat("iteration",i,"of",iterations,"\n")
    }

    for (p in seq_along(partition)) {

      ### print progress
      if (progress) {
        cat("- partition",p,"of",length(partition),"\n")
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
          attr(out, "gradient") <- attr(out, "gradient", exact = TRUE)[p_ind]
        }
        if (!is.null(attr(out, "hessian", exact = TRUE))) {
          attr(out, "hessian") <- attr(out, "hessian", exact = TRUE)[p_ind, p_ind]
        }
        return(out)
      }

      ### (try to) solve divided estimation problem
      conquer <- try_silent({
        do.call(what = optimx::optimx,
                args = list(par = estimate[p_ind],
                            fn = divide,
                            lower = f$lower[p_ind],
                            upper = f$upper[p_ind],
                            method = f$method,
                            itnmax = f$iterlim,
                            if(length(f$f_par) != 0) f$f_par))
      })

      ### evaluate
      if (!"ao_fail" %in% class(conquer)) {
        estimate[p_ind] <- conquer[, paste0("p",seq_along(p_ind))]
      }
      sequence <- rbind(sequence, c(i, p, estimate))
    }
  }

  ### end timer
  t_end <- Sys.time()

  ### compute optimal function value
  optimum <- f$f(estimate)

  ### prepare output
  output <- list(
    "optimum" = optimum,
    "estimate" = estimate,
    "sequence" = sequence,
    "time" = difftime(t_end, t_start, units = "secs")
  )

  class(output) <- "ao"

  ### return output
  return(output)
}

#' Print method for \code{ao}.
#'
#' This function is the print method for an object of class \code{ao}.
#'
#' @param x
#' An object of class \code{ao}.
#'
#' @param ...
#' Ignored.
#'
#' @export
#'
#' @noRd

print.ao <- function(x, ...) {
  cat("Optimum value:", zapsmall(x$optimum), "\n")
  cat("Optimum at:", zapsmall(x$estimate), "\n")
  cat("Optimization time:", signif(x$time, digits = 1), "seconds\n")
}
