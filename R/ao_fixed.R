#' @rdname ao
#' @export

ao_fixed <- function(
    f,
    initial,
    ...,
    fixed_partition = as.list(1:length(initial)),
    minimize = TRUE,
    iterations = 10,
    tolerance = 1e-6,
    joint_end = FALSE,
    verbose = FALSE) {
  ###
  objective <- Objective$new(f = f, npar = length(initial), ...)

  ###
  partition <- Partition$new(npar = length(initial), type = "fixed")$
    define_fixed_partition(fixed_partition = fixed_partition)$
    define_block_attribute("gradient", function(x, y) x[y])$
    define_block_attribute("hessian", function(x, y) x[y, y, drop = FALSE])

  ###
  optimizer <- Optimizer$new("stats::nlm")

  ###
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
