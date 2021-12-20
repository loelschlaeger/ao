#' Specify function.
#'
#' This function specifies the function that should be optimized.
#'
#' @param f
#' A function of \eqn{n} variables to be optimized.
#' @param npar
#' The number \eqn{n} of variables of \code{f}.
#'
#' @return
#' An object of class \code{ao_f}.
#' @export
#'
#' @examples
#' set_f(f = function(x) -x^2, npar = 1)

set_f <- function(f, npar) {
  ### input checks
  if(missing(f))
    stop("Please set 'f'.")
  if(!is.function(f))
    stop("'f' must be a function.")
  if(missing(npar))
    stop("Please set 'npar'.")
  if(!(length(npar) == 1 && is.number(npar)))
    stop("'npar' must be a number.")

  ### output
  out <- list(f = f,
              npar = npar)
  class(out) <- "ao_f"
  return(out)
}
