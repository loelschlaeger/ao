#' Specify optimizer.
#'
#' This function specifies the optimization routine.
#'
#' @param f
#' An object of class \code{ao_f}, i.e. the output of \code{\link{set_f}}.
#' @param optimizer
#' A function which can optimize \code{f}. The default is \code{\link[stats]{nlm}}.
#' @param f_arg
#' The name of the argument in \code{optimizer} that receives the function.
#' The default is \code{"f"}.
#' @param p_arg
#' The name of the argument in \code{optimizer} that receives the initial
#' parameter vector. The default is \code{"p"}.
#' @param ...
#' Further arguments to be passed to \code{optimizer}.
#' @param check
#' If \code{TRUE} checks the configuration.
#' This will take at most 10 seconds in most cases.
#' Set to \code{FALSE} if you are confident about the configuration to save
#' computation time.
#'
#' @return
#' An object of class \code{ao_optimizer}.
#' @export
#'
#' @examples
#' set_optimizer(
#'   f = set_f(f = function(x) x^2, npar = 1), optimizer = stats::optim,
#'   f_arg = "fn", p_arg = "par", lower = -2, upper = -1
#' )
set_optimizer <- function(f, optimizer = stats::nlm, f_arg = "f", p_arg = "p",
                          ..., check = TRUE) {
  ### input checks
  if (missing(f)) {
    stop("Please set 'f'.")
  }
  if (class(f) != "ao_f") {
    stop("'f' must be of class 'ao_f', i.e. the output of 'set_f'.")
  }
  if (class(optimizer) != "function") {
    stop("'optimizer' must be of class 'function'.")
  }
  if (!(length(f_arg) == 1 && is.character(f_arg))) {
    stop("'f_arg' must be a character.")
  }
  if (!(length(p_arg) == 1 && is.character(p_arg))) {
    stop("'p_arg' must be a character.")
  }

  ### read additional parameters for the optimizer
  optimizer_parameters <- list(...)

  ### define call to optimizer
  cto <- function(p) {
    x <- list(f$f, p)
    names(x) <- c(f_arg, p_arg)
    do.call(what = optimizer, args = c(x, optimizer_parameters))
  }

  ### configuration checks
  if (check) {
    check_runs <- 10
    f_success <- 0
    optimizer_success <- 0
    first_fail <- NULL
    for (i in 1:check_runs) {
      p <- stats::rnorm(f$npar)
      out_f <- timed(try_silent(f$f(p)), 1)
      if (class(out_f) == "ao_fail") {
        if (is.null(first_fail)) {
          first_fail <- out_f
        }
        next
      } else {
        f_success <- f_success + 1
        out_optimizer <- timed(try_silent(cto(p)), 1)
        if (class(out_optimizer) == "ao_fail") {
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
  out <- list(f = f, optimizer = optimizer, f_arg = f_arg, p_arg = p_arg, cto = cto)
  class(out) <- "ao_optimizer"
  return(out)
}

#' Print method for \code{ao_optimizer}.
#'
#' This function is the print method for an object of class \code{ao_optimizer}.
#'
#' @param x
#' An object of class \code{ao_optimizer}.
#'
#' @param ...
#' Ignored.
#'
#' @export
#'
#' @noRd

print.ao_optimizer <- function(x, ...) {
  cat("<ao_optimizer>")
}
