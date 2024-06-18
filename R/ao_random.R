#' @rdname ao
#' @export

ao_random <- function(
    f,
    initial,
    ...,
    new_block_probability = 0.3,
    minimum_block_number = 2,
    minimize = TRUE,
    iteration_limit = Inf,
    tolerance_value = 1e-6,
    verbose = FALSE) {
  ### define 'Objective' object
  objective <- Objective$new(f = f, npar = length(initial), ...)

  ### define 'Partition' object
  partition <- Partition$new(npar = length(initial), type = "random")$
    define_fixed_partition(fixed_partition = fixed_partition)$
    define_block_attribute("gradient", function(x, y) x[y])$
    define_block_attribute("hessian", function(x, y) x[y, y, drop = FALSE])
  partition$new_block_probability <- new_block_probability
  partition$minimum_block_number <- minimum_block_number

  ### define 'Optimizer' object
  optimizer <- Optimizer$new("stats::nlm")

  ### define 'Procedure' object
  procedure <- Procedure$new(
    verbose = verbose,
    minimize = minimize,
    iteration_limit = iteration_limit,
    tolerance_value = tolerance_value
  )

  ### perform alternating optimization
  ao(
    objective = objective,
    partition = partition,
    optimizer = optimizer,
    initial = initial,
    procedure = procedure
  )
}
