#' Specify function.
#'
#' This function specifies the function to be optimized.
#'
#' @param f
#' A function to be optimized, returning a single numeric value.
#' Its first argument should be a numeric vector of length \code{npar}.
#' Additional arguments can be specified via the \code{...} argument.
#' Gradient or Hessian of \code{f} can be specified via attributes
#' \code{gradient} and \code{hessian} for the function value.
#' They are used for optimization if specified.
#' @param ...
#' Additional arguments to be passed to \code{f}.
#' @param npar
#' The number of variables of \code{f}.
#' @param lower
#' Lower bounds on the variables, which can be a single numeric value
#' (a joint bound for all parameters) or a numeric vector of length \code{npar}
#' (for individual bounds).
#' @param upper
#' Upper bounds on the variables, analogue to \code{lower}.
#' @param iterlim
#' The maximum number of iterations for the numerical optimizer for each
#' sub-problem. No limit per default.
#' @param check
#' If \code{TRUE} checks the configuration.
#' This will take at most 20 seconds in most cases.
#' Set to \code{FALSE} if you are confident about the configuration to save
#' computation time.
#'
#' @return
#' An object of class \code{ao_f}.
#'
#' @export
#'
#' @examples
#' himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
#' set_f(f = himmelblau, npar = 2, lower = -5, upper = 5)
set_f <- function(f, ..., npar, lower = -Inf, upper = Inf, iterlim = NULL,
                  check = FALSE) {

  ### input checks
  if (missing(f)) {
    stop("Please set 'f'.", .call = FALSE)
  }
  if (!is.function(f)) {
    stop("'f' must be a function.", .call = FALSE)
  }
  if (missing(npar)) {
    stop("Please set 'npar'.", .call = FALSE)
  }
  if (!(length(npar) == 1 && is_number(npar))) {
    stop("'npar' must be a number.", .call = FALSE)
  }
  if (!is.numeric(lower)) {
    stop("'lower' must be numeric.", .call = FALSE)
  }
  if (length(lower) == 1) {
    lower <- rep(lower, npar)
  }
  if (length(lower) != npar) {
    stop("'lower' must be of length 1 or 'npar'.", .call = FALSE)
  }
  if (!is.numeric(upper)) {
    stop("'upper' must be numeric.", .call = FALSE)
  }
  if (length(upper) == 1) {
    upper <- rep(upper, npar)
  }
  if (length(upper) != npar) {
    stop("'upper' must be of length 1 or 'npar'.", .call = FALSE)
  }
  if (any(upper < lower)) {
    stop("(Each element of) 'upper' must be greater than 'lower'.", .call = FALSE)
  }
  if (!is.null(iterlim)) {
    if (length(iterlim) != 1 || !is_number(iterlim)) {
      stop("'iterlim' must be a number.", .call = FALSE)
    }
  }
  if (!is.logical(check)) {
    stop("'stop' must be a boolean.", .call = FALSE)
  }
  f_par <- list(...)
  constrains <- if (any(lower != -Inf) || any(upper != Inf)) TRUE else FALSE
  method <- ifelse(constrains, "L-BFGS-B", "nlm")

  ### define call to optimizer
  cto <- function(p) {
    do.call(
      what = optimx::optimx,
      args = list(
        par = p,
        fn = f,
        lower = lower,
        upper = upper,
        method = method,
        itnmax = iterlim,
        if (length(f_par) != 0) f_par
      )
    )
  }

  ### configuration checks
  if (check) {
    check_runs <- 10
    f_success <- 0
    optimizer_success <- 0
    first_fail <- NULL
    for (i in 1:check_runs) {
      if (constrains) {
        p <- stats::runif(npar, lower, upper)
      } else {
        p <- stats::rnorm(npar)
      }
      out_f <- timed(try_silent(f(p)), 1)
      if ("ao_fail" %in% class(out_f)) {
        if (is.null(first_fail)) {
          first_fail <- out_f
        }
        next
      } else {
        f_success <- f_success + 1
        out_optimizer <- timed(try_silent(cto(p)), 1)
        if ("ao_fail" %in% class(out_optimizer)) {
          if (is.null(first_fail)) {
            first_fail <- out_optimizer
          }
          next
        } else {
          optimizer_success <- optimizer_success + 1
        }
      }
    }
    if (f_success == 0 || optimizer_success == 0) {
      stop("Configuration failed.\nFirst error was: ", first_fail, call. = FALSE)
    } else if (f_success < check_runs || optimizer_success < check_runs) {
      warning(check_runs - f_success, " of ", check_runs, " random calls of 'f' failed. ",
        check_runs - optimizer_success, " of ", check_runs, " calls of 'optimizer' failed.\n",
        "First error was: ", first_fail,
        call. = FALSE
      )
    } else {
      message("Configuration checked.")
    }
  }

  ### output
  out <- list(
    f = f,
    npar = npar,
    lower = lower,
    upper = upper,
    iterlim = iterlim,
    constrains = constrains,
    method = method,
    f_par = f_par
  )
  class(out) <- "ao_f"
  return(out)
}

#' Print method for \code{ao_f}.
#'
#' This function is the print method for an object of class \code{ao_f}.
#'
#' @param x
#' An object of class \code{ao_f}.
#'
#' @param ...
#' Ignored.
#'
#' @export
#'
#' @noRd

print.ao_f <- function(x, ...) {
  cat("<ao_f>")
}
