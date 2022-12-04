#' Check for number
#'
#' @description
#' This function checks if the input \code{x} is a (vector of) number(s), i.e.
#' a (vector of) positive integer value(s).
#'
#' @param x
#' A (vector of) value(s).
#'
#' @return
#' A logical vector of the same length as \code{x}.
#'
#' @examples
#' \dontrun{
#' is_number(c(0, 1, 1.5))
#' }
#'
#' @keywords
#' internal utils

is_number <- function(x) {
  sapply(x, function(x) is.numeric(x) && x > 0 && x %% 1 == 0, USE.NAMES = F)
}

