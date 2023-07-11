#' Alternating Optimization
#'
#' @description
#' Alternating optimization of (high-dimensional) functions is an iterative
#' procedure for optimizing jointly over all parameters by alternately
#' optimizing parameter subsets.
#'
#' @docType package
#'
#' @name ao
#'
#' @keywords
#' internal
#'
#' @import optimizeR
"_PACKAGE"

#' @noRd
#' @keywords
#' internal

ao_stop <- function(event, debug = character(), call. = FALSE) {
  msg <- paste(event, debug, sep = "\n", collapse = "")
  stop(msg, call. = call.)
}

#' @noRd
#' @keywords
#' internal

ao_warn <- function(
    event, debug = character(), call. = FALSE, immediate. = FALSE
  ) {
  msg <- paste(event, debug, sep = "\n", collapse = "")
  warning(msg, call. = call., immediate. = immediate.)
}
