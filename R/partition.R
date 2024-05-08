#' @title Partition Object
#'
#' @description
#' Partition of the target argument for alternating optimization.
#'
#' @export

Partition <- R6::R6Class("Partition",

  cloneable = FALSE,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param n (`integer(1)`)\cr
    #'   The length of the target vector.
    #' @param type (`character(1)`)\cr
    #'   - `"random"`
    #'   - `"fixed"`
    #'   - `"sequential"`
    #'   - `"none"`
    initialize = function(n, type = "random") {

    },

    print = function() {

    },

    define = function() {

    },

    validate = function() {

    }

  ),

  active = list(

    #' @field partitions (`list`)\cr
    #' description
    partitions = function() {

    },

    #' @field random (`numeric(1)`)\cr
    #' description
    random = function() {

    }

  ),

  private = list(

    .partitions = list(),
    .random = numeric(),
    .all = list(),

    .generate = function() {

    }

  )
)


#' rndblocks makes randomized blocks

#' @param x is a sequence starting from 1 to k (the number of parameters)

#' @param p is the probability of starting a new block

#' @export

#' @author Siddhartha Chib

rndblocks <- function(x = x,p = .3) {

  x = sample(x,replace = F);
  n = length(x);
  y = sample(0:1,n,replace = T,prob = c(1-p,p))
  y[1] = 1;
  ind = which(y %in% 1)
  B = length(ind);
  blocksls = vector("list",B)
  for (j in 1:B) {
    s = ind[j];
    if (j < B) {
      e = ind[(j+1)]-1;
    } else {
      e = n;
    }
    xj = x[s:e]
    blocksls[[j]] = xj[order(xj)]
  }
  return(blocksls)
}

