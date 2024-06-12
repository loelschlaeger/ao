rm(list=ls())
load_all()

himmelblau <- function(x) {
  value <- (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  gradient <- c(
    4 * x[1] * (x[1]^2 + x[2] - 11) + 2 * (x[1] + x[2]^2 - 7),
    2 * (x[1]^2 + x[2] - 11) + 4 * x[2] * (x[1] + x[2]^2 - 7)
  )
  hessian <- matrix(
    c(
      12 * x[1]^2 + 4 * x[2] - 42,
      4 * (x[1] + x[2]),
      4 * (x[1] + x[2]),
      4 * x[1] + 12 * x[2]^2 - 26
    ),
    nrow = 2, ncol = 2
  )
  structure(value, "gradient" = gradient, "hessian" = hessian)
}

f = himmelblau
initial = c(0, 0)
fixed_partition = list(1, 2)
minimize = TRUE
iteration_limit = Inf
tolerance_value = 1e-6
verbose = TRUE

### define 'Objective' object
objective <- Objective$new(f = f, npar = length(initial))

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

self <- procedure
private <- self$.__enclos_env__$private

### perform alternating optimization
# ao(
#   objective = objective,
#   partition = partition,
#   optimizer = optimizer,
#   initial = initial,
#   procedure = procedure
# )



