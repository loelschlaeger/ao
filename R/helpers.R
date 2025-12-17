# TODO: add doc + tests

split_by_target <- function(estimate, target, npar){
  out = list()
  idx = 1L
  for(i in seq_along(target)){
    k = as.integer(npar[i])
    out[[target[i]]] = estimate[idx:(idx+k-1L)]
    idx = idx+k
  }
  out
}

# TODO: improve doc + add tests

#' Generated randomized blocks.
#' @param x The parameter indices.
#' @param p The probability to generate a new block.
#' @param min The minimum number of blocks.
#' @return TODO
#' @author Siddhartha Chib

generate_random_partition = function(
    x = seq_len(self$npar),
    p = self$new_block_probability,
    min = self$minimum_block_number
) {
  if (min == length(x)) {
    return(as.list(x))
  }
  x <- sample(x, replace = FALSE)
  n <- length(x)
  y <- sample(0:1, n, replace = TRUE, prob = c(1 - p, p))
  y[1] <- 1
  ind <- which(y == 1)
  if (length(ind) < min) {
    ind <- sort(c(ind, sample(which(y == 0), size = min - length(ind))))
  }
  B <- length(ind)
  blocks <- vector("list", B)
  for (j in seq_len(B)) {
    s <- ind[j]
    e <- if (j < B) ind[j + 1] - 1 else n
    xj <- x[s:e]
    blocks[[j]] <- xj[order(xj)]
  }
  blocks
}

# TODO: add doc + tests

merge_results <- function() {

}

