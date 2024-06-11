#' @rdname ao
#' @export

ao_fixed <- function(
    f,
    initial,
    ...,
    fixed_partition = as.list(1:length(initial)),
    minimize = TRUE,
    iterations_limit = 10,
    tolerance_value = 1e-6,
    verbose = FALSE
  ) {

  ### define 'Objective' object
  objective <- Objective$new(f = f, npar = length(initial), ...)

  ### define 'Partition' object
  partition <- Partition$new(npar = length(initial), type = "fixed")$
    define_fixed_partition(fixed_partition = fixed_partition)$
    define_block_attribute("gradient", function(x, y) x[y])$
    define_block_attribute("hessian", function(x, y) x[y, y, drop = FALSE])

  ### define 'Optimizer' object
  optimizer <- Optimizer$new("stats::nlm")

  ### define 'Procedure' object
  procedure <- Procedure$new(
    verbose = verbose, minimize = minimize, iteration_limit = iteration_limit,
    tolerance_value = tolerance_value, tolerance_parameter = 0
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
