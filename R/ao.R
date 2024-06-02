#' Alternating Optimization
#'
#' @description
#' Performing alternating optimization of the function \code{f}.
#'
#' - `ao` is the most general function
#' - `ao_fixed` is the special case of a fixed partition
#' - `ao_random` is the special case of random partitions
#'
#' @param objective (`numeric()`)\cr
#' The definition of the objective function to be optimized via alternating
#' optimization, can be created via \code{\link[optimizeR]{Objective}}.
#'
#' @param partition (`numeric()`)\cr
#' The definition of the target argument partition for alternating optimization,
#' can be created via \code{\link{Partition}}.
#'
#' @param optimizer (`Optimizer`)\cr
#' The definition of the base optimizer that solves the optimization problems in
#' the partitions, can be created via \code{\link[optimizeR]{Optimizer}}.
#'
#' @param initial (`numeric()`)\cr
#' The starting parameter values for the optimization.
#'
#' @param minimize (`logical(1)`)\cr
#' Perform minimization? Alternatively, maximization is performed.
#'
#' @param iterations (`integer(1)`)\cr
#' The maximum number of iterations through the parameter partition before the
#' alternating optimization process is terminated. Can also be \code{Inf}, in
#' which case \code{tolerance} is responsible for the termination.
#'
#' @param tolerance (`numeric(1)`)\cr
#' A non-negative tolerance value. The alternating optimization terminates
#' prematurely (i.e., before \code{iterations} is reached) if the euclidean
#' distance between the current estimate and the one from the last iteration is
#' smaller than \code{tolerance}.
#'
#' @param joint_end (`logical(1)`)\cr
#' Optimize the parameter set jointly after the alternating optimization process
#' is terminated?
#'
#' @param verbose (`logical(1)`)\cr
#' Print tracing details during the alternating optimization process?
#'
#' @param f (`function`)\cr
#' A \code{function} to be optimized, returning a single \code{numeric}.
#' The first argument of \code{f} must be a \code{numeric} of the same length as
#' \code{initial} followed by any other arguments specified by the \code{...}
#' argument.
#'
#' @param ...
#' Additional arguments to be passed to \code{f}.
#'
#' @param fixed_partition (`list()`)\cr
#' A \code{list} of vectors of indices of \code{initial}, specifying the
#' partition of the parameter vector in the alternating optimization process.
#' The default is \code{as.list(1:length(initial))}, i.e. each parameter is
#' optimized separately. Parameter indices can be members of multiple blocks.
#'
#' @param new_block_probability (`numeric(1)`)\cr
#' The probability for a new parameter block in random partitions.
#' Values close to 0 result in larger parameter blocks, values close to 1
#' result in smaller parameter blocks.
#'
#' @param minimum_block_number (`integer(1)`)\cr
#' The minimum number of blocks in random partitions.
#'
#' @return
#' A \code{list} with the elements
#' * \code{estimate}, the optimal parameter vector found,
#' * \code{value}, the value of \code{f} at \code{estimate},
#' * \code{sequence}, a \code{data.frame} of the function values, estimates and
#'   computation times in the single iterations and parameter blocks,
#' * and \code{seconds}, the overall computation time in seconds.
#'
#' @examples
#' # definition of the 'Himmelblau' objective function with two parameters
#' objective <- Objective$new(
#'   f = function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2,
#'   npar = 2
#' )
#'
#' # definition of the partition: one parameter conditional on the other
#' partition <- Partition$new(npar = 2, type = "sequential")
#'
#' # definition of the optimizer with parameter restriction: -5 <= x_1, x_2 <= 5
#' optimizer <- Optimizer$new(
#'   which = "stats::optim", lower = -5, upper = 5, method = "L-BFGS-B"
#' )
#'
#' # alternating optimization
#' ao(
#'   objective = objective,
#'   partition = partition,
#'   optimizer = optimizer,
#'   initial = c(0, 0), # initial parameter values
#'   minimize = TRUE, # minimization
#'   iterations = Inf, # no restriction on the number of iterations
#'   tolerance = 1e-6, # stop if change in parameters is within tolerance
#'   joint_end = TRUE, # finally perform joint optimization
#'   verbose = TRUE # print progress
#' )
#'
#' @export

ao <- function(
    objective,
    partition = Partition$new(npar = sum(objective$npar), type = "random"),
    optimizer = Optimizer$new("stats::optim"),
    initial = stats::rnorm(sum(objective$npar)),
    procedure = Procedure$new(verbose = TRUE, minimize = TRUE)) {
  ### input checks and preliminary preparations
  ao_input_checks(
    objective = objective, partition = partition, optimizer = optimizer,
    initial = initial, procedure = procedure
  )
  npar <- partition$npar
  block_objective <- ao_build_block_objective()
  procedure$initialize_details(
    initial = initial, value = objective$evaluate(initial), npar = npar
  )

  ### alternating optimization
  procedure$info("start alternating optimization")
  while (TRUE) {
    ### check stopping criteria
    if (procedure$stopping) break
    procedure$next_iteration()

    ### generate partition
    next_partition <- partition$get()

    ### optimize over each parameter block in partition
    for (block in partition$get()) {
      procedure$next_block()

      ### optimize block objective function
      block_objective_out <- optimizer$optimize(
        objective = block_objective(block),
        initial = procedure$parameter[block],
        direction = ifelse(procedure$minimize, "min", "max"),
        theta_rest = procedure$parameter[-block]
      )

      ### check acceptance and update
      procedure$update_details(
        value = block_objective_out[["value"]],
        parameter_block = block_objective_out[["parameter"]],
        seconds = block_objective_out[["seconds"]]
      )
    }
  }

  ### return results
  procedure$output
}
