### function that performs alternating optimization
### target: function, expression to be optimized
### groups: list, containing vectors of parameter indices that get grouped
### runs: integer, number of optimization runs
### initial: numeric vector, containing initial values 
### progress: boolean, determining wheter progress should be printed
### ...: arguments that get passed on to nlm

ao = function(target, groups, runs, initials, progress=FALSE, ...){
  
  ### read additional inputs
  additional_inputs = as.list(substitute(list(...)))[-1L]
  
  ### check inputs 
  stopifnot(is.function(target))
  
  ### read inputs
  no_groups = length(groups)
  
  ### build initial values
  estimate = initial
  
  for(run in 1:runs){
    
    ### select group
    selected = (run-1)%%no_groups + 1
    
    ### save fixed values
    fixed_values = estimate[unlist(groups[-selected])]
    
    ### divide estimation problem
    divide = function(theta_small) {
      theta = numeric(length(unlist(groups)))
      theta[groups[[selected]]] = theta_small
      theta[-groups[[selected]]] = fixed_values
      return(target(theta))
    }
    
    ### (try to) solve estimation problem
    conquer = suppressWarnings(try(
      {
        f = function(theta_small) divide(theta_small)
        p = runif(groups[[selected]])
        do.call(what = nlm, args = c(f = f, p = list(p), additional_inputs))
      }
      ,silent = TRUE))
    if(class(conquer) == "try-error") next
    
    ### save estimates
    estimate[groups[[selected]]] = conquer$estimate
    
    ### print progress
    if(progress) cat(sprintf("%.0f%% \r",(run-1)/runs*100))
  }
  
  ### return final statement
  if(progress) cat("Done.\n")
  
  ### return estimates
  return(estimate)
}
