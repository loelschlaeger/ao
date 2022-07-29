#' ao: Alternating Optimization
#'
#' @description
#' Alternating optimization of (high-dimensional) functions is an iterative
#' procedure for minimizing (or maximizing) jointly over all parameters by
#' alternately optimizing parameter subsets.
#'
#' @docType package
#' @name ao
#' @keywords
#' internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

#' @noRd
#' @keywords
#' internal

.onAttach <- function(lib, pkg) {
  msg <- c(
    paste0(
      "Thanks for using {ao} ", utils::packageVersion("ao")
    ),
    ", happy alternating optimization!\n",
    "See https://loelschlaeger.github.io/ao for help.\n",
    "Type 'citation(\"ao\")' for citing this R package."
  )
  packageStartupMessage(msg)
  invisible()
}

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

ao_warn <- function(event, debug = character(), call. = FALSE, immediate. = FALSE) {
  msg <- paste(event, debug, sep = "\n", collapse = "")
  warning(msg, call. = call., immediate. = immediate.)
}
