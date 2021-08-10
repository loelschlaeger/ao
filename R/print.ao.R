#' Print method for \code{ao}
#' @param x
#' The output of \code{\link{ao}}, which is an object of class \code{ao}.
#' @param ...
#' ignored
#' @export

print.ao = function(x,...) {
  cat("Alternating optimization\n")
  cat(if(x$minimize) "Minimum value:",
      if(!x$minimize) "Maximum value:",
      x$optimum,"\n")
  cat(if(x$minimize) "Minimum at:",
      if(!x$minimize) "Maximum at:",
      x$estimate,"\n")
  cat("computation time:",signif(x$time,digits=1),"seconds\n")
}
