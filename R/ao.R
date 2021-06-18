#' @title
#' Alternating Optimization
#' @description
#' This function performs alternating optimization on the function \code{f}.
#' @param f
#' A function of \eqn{n} variables to be optimized.
#' @param npar
#' An integer, the number \eqn{n} of variables of \code{f}.
#' @param groups
#' A list of vectors of parameter indices \eqn{1,...,n} of \code{f}.
#' This determines the grouping of parameters.
#' Indices can be present in multiple groups.
#' @param sequence
#' A vector of indices of the list \code{groups}.
#' This determines the sequence in which parameter groups get optimized.
#' @param iterlims
#' A vector of iteration limits for the different optimization.
#' Must have the same length as \code{sequence}.
#' If not supplied, the default \link[stats]{nlm} setting is used.
#' @param initial
#' A vector of length \eqn{n} of initial parameter values.
#' If not supplied, they are randomly drawn.
#' @param minimize
#' A boolean, determining whether to minimize (\code{minimize = TRUE})
#' or to maximize (\code{minimize = FALSE}) the function \code{f}.
#' @param progress
#' A boolean, determining whether progress should be printed.
#' @param ...
#' Arguments that get passed on to \link[stats]{nlm} (except for \code{iterlim}).
#' @value
#' The point at which the optimum of \code{f} is obtained.
#' @export
#' @examples
#' ao(f = function(x) 3*x[1]^2 + 2*x[1]*x[2] + x[2]^2 - 5*x[1] + 2,
#'    npar = 2,
#'    groups = list(1,2),
#'    sequence = rep(c(1,2),10))

ao = function(f, npar, groups, sequence, iterlims, initial, minimize = TRUE, progress = FALSE, ...){

  ### read additional inputs and remove 'iterlim' (if submitted)
  nlm_parameters = as.list(substitute(list(...)))[-1L]
  nlm_parameters[["iterlim"]] = NULL

  ### function that checks if value is an integer
  is.integer = function(x) all(is.numeric(x)) && all(x>0) && all(x%%1==0)

  ### check inputs
  if(missing(f))
    stop("Please set 'f'.")
  if(!is.function(f))
    stop("'f' must be a function.")
  if(missing(npar))
    stop("Please set 'npar'.")
  if(!is.integer(npar))
    stop("'npar' must be an integer.")
  if(missing(groups))
    stop("Please set 'groups'.")
  if(!is.list(groups))
    stop("'groups' must be a list.")
  if(!is.integer(unlist(groups)))
    stop("'groups' must be a list of integers.")
  if(any(!unlist(groups) %in% seq_len(npar)))
    stop("'groups' contains values that are not parameter indices of 'f'.")
  if(missing(sequence))
    stop("Please set 'sequence'.")
  if(!is.integer(sequence))
    stop("'sequence' must be a vector of integers.")
  if(any(!sequence %in% seq_len(length(groups))))
    stop("'sequence' contains values that or not indices of 'groups.'")
  if(any(!seq_len(npar) %in% unlist(groups[unique(sequence)])))
    warning(paste("Parameter(s)",
                  paste(setdiff(seq_len(npar),unlist(groups[unique(sequence)])),collapse=", "),
                  "did not get optimized."))
  if(!missing(iterlims)){
    if(length(sequence) != length(iterlims))
      stop("'sequence' and 'iterlims' must be of the same length.")
    if(!is.integer(iterlims))
      stop("'iterlims' must be a vector of integers.")
  }
  if(!missing(initial)){
    if(!is.numeric(initial))
      stop("'initial' must be a numeric vector.")
    if(length(initial) != npar)
      stop("'initial' must be a numeric vector of length 'npar.'")
  }
  if(!is.logical(minimize))
    stop("'minimize' must be a boolean.")
  if(!is.logical(progress))
    stop("'progress' must be a boolean.")

  ### read inputs
  no_groups = length(groups)

  ### build initial values
  if(missing(initial)) initial = rnorm(npar)
  estimate = initial

  for(run in 1:length(sequence)){

    ### print progress
    if(progress) cat(sprintf("%.0f%% \r",(run-1)/length(sequence)*100))

    ### select group
    selected = sequence[run]

    ### save fixed values
    fixed_values = estimate[unlist(groups[-selected])]

    ### divide estimation problem
    divide = function(theta_small) {
      theta = numeric(length(unlist(groups)))
      theta[groups[[selected]]] = theta_small
      theta[-groups[[selected]]] = fixed_values
      out = f(theta)
      if(!minimize)
        out = -out
      if(!is.null(attr(out,"gradient",exact=TRUE)))
        attr(out,"gradient") = attr(out,"gradient",exact=TRUE)[groups[[selected]]]
      if(!is.null(attr(out,"hessian",exact=TRUE)))
        attr(out,"hessian") = attr(out,"hessian",exact=TRUE)[groups[[selected]],groups[[selected]]]
      return(out)
    }

    ### (try to) solve divided estimation problem
    conquer = suppressWarnings(try(
      {
        p = runif(length(groups[[selected]]))
        args = list(f = divide, p = p)
        if(!missing(iterlims))
          if(!is.na(iterlims[run]))
            args[["iterlim"]] = iterlims[run]
        do.call(what = nlm, args = c(args, nlm_parameters))
      }
      ,silent = TRUE))
    if(class(conquer) == "try-error") next

    ### save estimate
    estimate[groups[[selected]]] = conquer$estimate

  }

  ### name estimates
  names(estimate) = paste("x",seq_len(npar),sep="_")

  ### return estimate
  return(estimate)
}
