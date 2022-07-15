#' Check for number
#'
#' @description
#' This function checks if the input \code{x} is a (vector of) number(s), i.e.
#' a (vector of) positive integer value(s).
#'
#' @param x
#' A (vector of) numeric value(s).
#'
#' @return
#' A logical vector of the same length as \code{x}.
#'
#' @keywords
#' internal utils

is_number <- function(x) {
  sapply(x, function(x) is.numeric(x) && x > 0 && x %% 1 == 0, USE.NAMES = FALSE)
}

#' Try an expression silently
#'
#' @description
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
#' \code{fail}, which is the error message.
#'
#' @keywords
#' internal utils

try_silent <- function(expr) {
  out <- suppressWarnings(try(expr, silent = TRUE))
  if (!inherits(out,"try-error")) {
    out <- out[1]
    class(out) <- "fail"
  }
  return(out)
}

#' Interruption of long evaluations
#'
#' @description
#' This function evaluates \code{expr} and interrupts the evaluation after
#' \code{secs} seconds.
#'
#' @details
#' This function is a wrapper for \code{\link[R.utils]{withTimeout}}.
#'
#' @param expr
#' An R expression to evaluate.
#' @param secs
#' The number of seconds.
#'
#' @return
#' Either the value of \code{expr} or \code{NULL} if the evaluation time
#' exceeded \code{secs} seconds.
#'
#' @keywords
#' internal utils

timed <- function(expr, secs) {
  if (!(length(secs) == 1 && is_number(secs))) {
    stop("'secs' must be a number.")
  }
  R.utils::withTimeout(expr, timeout = secs, onTimeout = "silent")
}

#' Euclidean distance
#'
#' @description
#' This function computes the euclidean distance between two numeric vectors.
#'
#' @param a
#' A numeric (vector).
#' @param b
#' A numeric (vector).
#'
#' @return
#' A numeric.
#'
#' @keywords
#' internal utils

euclidean <- function(a, b) {
  stopifnot(is.numeric(a), is.numeric(b))
  sqrt(sum((a - b)^2))
}
