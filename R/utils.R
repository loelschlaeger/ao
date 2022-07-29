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
