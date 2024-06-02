#' @rdname ao
#' @export

ao_random <- function(
    f,
    initial,
    ...,
    new_block_probability = 0.3,
    minimum_block_number = 1,
    minimize = TRUE,
    iterations = 10,
    tolerance = 1e-6,
    joint_end = FALSE,
    verbose = FALSE) {
  objective <- Objective$new(f = f, npar = length(initial), ...)
  partition <- Partition$new(npar = length(initial), type = "random")
  partition$new_block_probability <- new_block_probability
  partition$minimum_block_number <- minimum_block_number
  optimizer <- Optimizer$new("stats::nlm")
  ao(
    objective = objective,
    partition = partition,
    optimizer = optimizer,
    initial = initial,
    minimize = minimize,
    iterations = iterations,
    tolerance = tolerance,
    joint_end = joint_end,
    verbose = verbose
  )
}
