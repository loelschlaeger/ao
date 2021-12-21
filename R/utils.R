#' Check for number.
#'
#' This function checks if the input \code{x} is a (vector of) number(s), i.e.
#' a (vector of) positive integer value(s).
#'
#' @param x
#' A (vector of) numeric value(s).
#'
#' @return
#' A logical vector of the same length as \code{x}.
#'
#' @export
#'
#' @examples
#' is.number(c(0, 1, 1.5))
is.number <- function(x) {
  sapply(x, function(x) is.numeric(x) && x > 0 && x %% 1 == 0, USE.NAMES = FALSE)
}

#' Try an expression silently.
#'
#' This function tries to execute \code{expr} and returns a string with the
#' error message if the execution failed.
#'
#' @details
#' This function is a wrapper for \code{\link[base]{try}}.
#'
#' @param expr
#' An R expression to try.
#'
#' @return
#' Either the value of \code{expr} or in case of a failure an object of class
#' \code{ao_fail}, which is the error message.
#'
#' @export
#'
#' @examples
#' try_silent(log(1))
#' try_silent(log("1"))
try_silent <- function(expr) {
  out <- suppressWarnings(try(expr, silent = TRUE))
  if (class(out) == "try-error") {
    out <- out[1]
    class(out) <- "ao_fail"
  }
  return(out)
}

#' Print method for \code{ao_fail}.
#'
#' This function is the print method for an object of class \code{ao_fail}.
#'
#' @param x
#' An object of class \code{ao_fail}.
#'
#' @param ...
#' Ignored.
#'
#' @export
#'
#' @noRd

print.ao_fail <- function(x, ...) {
  cat(x)
}

#' Interruption of long evaluations.
#'
#' This function evaluates \code{expr} and interrupts the evaluation after
#' \code{secs} seconds.
#'
#' @details
#' This function is a wrapper for \code{\link[R.utils]{withTimeout}}.
#'
#' @param expr
#' An R expression to evaluate.
#'
#' @param secs
#' The number of seconds.
#'
#' @return
#' Either the value of \code{expr} or \code{NULL} if the evaluation time
#' exceeded \code{secs} seconds.
#'
#' @export
#'
#' @examples
#' timed(Sys.sleep(1.1), 1)
timed <- function(expr, secs) {
  if (!(length(secs) == 1 && is.number(secs))) {
    stop("'secs' must be a number.")
  }
  R.utils::withTimeout(expr, timeout = secs, onTimeout = "silent")
}
