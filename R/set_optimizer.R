#' Specify optimizer.
#'
#' This function specifies the optimization routine.
#'
#' @param f
#' An object of class \code{ao_f}, i.e. the output of \code{\link{set_f}}.
#' @param optimizer
#' A function which can optimize \code{f}. The default is \code{\link[stats]{nlm}}.
#' @param f_arg
#' The position of the argument in \code{optimizer} that receives the function.
#' The default is \code{1}.
#' @param p_arg
#' The position of the argument in \code{optimizer} that receives the initial
#' parameter vector. The default is \code{2}.
#' @param ...
#' Further arguments to be passed to \code{optimizer}.
#' @param check
#' If \code{TRUE} checks the configuration. This will take at most 10 seconds.
#' Set to \code{FALSE} if you are confident about the configuration to save
#' computation time.
#'
#' @return
#' An object of class \code{ao_optimizer}.
#' @export
#'
#' @examples
#' set_optimizer(f = set_f(f = function(x) x^2, npar = 1), optimizer = stats::optim,
#'               f_arg = 2, p_arg = 1, lower = -2, upper = -1)

set_optimizer <- function(f, optimizer = stats::nlm, f_arg = 1, p_arg = 2, ...,
                          check = TRUE) {
  ### input checks
  if(missing(f))
    stop("Please set 'f'.")
  if(class(f) != "ao_f")
    stop("'f' must be of class 'ao_f', i.e. the output of 'set_f'.")
  if(class(optimizer) != "function")
    stop("'optimizer' must be of class 'function'.")
  if(!(length(f_arg) == 1 && is.number(f_arg)))
    stop("'f_arg' must be a number.")
  if(!(length(p_arg) == 1 && is.number(p_arg)))
    stop("'p_arg' must be a number.")
  if(p_arg == f_arg)
    stop("'f_arg' and 'p_arg' must be different numbers.")

  ### configuration checks
  if(check) {
    # R.utils::withTimeout
  }

  ### output
  out <- list()
  class(out) <- "ao_optimizer"
  return(out)
}
