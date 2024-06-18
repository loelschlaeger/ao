#' Alternating Optimization
#'
#' @description
#' Alternating optimization is an iterative procedure for optimizing a
#' real-valued function jointly over all its parameters by alternating
#' restricted optimization over parameter partitions.
#'
#' - `ao` is the most general API
#' - `ao_fixed` is the special case of a parameter partition that remains fixed
#'   during the alternating optimization procedure
#' - `ao_random` is the special case of random partitions
#'
#' @param objective (`Objective`)\cr
#' The objective function to be optimized via alternating optimization.
#' Can be created via \code{\link[optimizeR]{Objective}}.
#'
#' @param partition (`Partition`)\cr
#' The parameter partition for alternating optimization.
#' Can be created via \code{\link{Partition}}.
#'
#' @param optimizer (`Optimizer`)\cr
#' The optimizer for solving the sub-problems.
#' Can be created via \code{\link[optimizeR]{Optimizer}}.
#'
#' @param initial (`numeric()`)\cr
#' The starting parameter values.
#'
#' @param procedure (`Procedure`)\cr
#' The alternating optimization procedure.
#' Can be created via \code{\link{Procedure}}.
#'
#' @param f (`function`)\cr
#' A \code{function} to be optimized, returning a single \code{numeric}.
#' The first argument of \code{f} must be a \code{numeric} of the same length as
#' \code{initial}, followed by any other arguments specified by the \code{...}
#' argument.
#'
#' @param ...
#' Additional arguments to be passed to \code{f}.
#'
#' @param fixed_partition (`list()`)\cr
#' A \code{list} of vectors of indices of \code{initial}, specifying the
#' partition of the parameter vector in the alternating optimization process.
#' The default is \code{as.list(1:length(initial))}, i.e. each parameter is
#' optimized separately.

#' @param minimize (`logical(1)`)\cr
#' Whether to minimize during the alternating optimization process.
#' If \code{FALSE}, maximization is performed.
#'
#' @param iteration_limit (`integer(1)` or `Inf`)\cr
#' The maximum number of iterations through the parameter partition before
#' the alternating optimization process is terminated.
#' Can also be `Inf` for no iteration limit.
#'
#' @param tolerance_value (`numeric(1)`)\cr
#' A non-negative tolerance value. The alternating optimization terminates
#' if the absolute difference between the current function value and the one
#' from the last iteration is smaller than \code{tolerance_value}.
#' Can be `0` for no value threshold.
#'
#' @field verbose (`logical(1)`)\cr
#' Whether to print tracing details during the alternating optimization
#' process.
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
#' A \code{list} with the following elements:
#' * \code{estimate} is the parameter vector at termination.
#' * \code{value} is the function value at termination.
#' * \code{details} is a `data.frame` with full information about the procedure:
#'   For each iteration (column `iteration`) it contains the function value
#'   (column `value`), parameter values (columns starting with `p` followed by
#'   the parameter index), the active parameter block (columns starting with `b`
#'   followed by the parameter index, where `1` stands for a parameter contained
#'   in the active parameter block and `0` if not), computation times in seconds
#'   (column `seconds`), and a code that summarizes whether the update got
#'   accepted (column `update_code`, where `0` stands for an accepted update,
#'   `1` for a rejected update due to an error when solving the sub-problem, and
#'   `2` for a rejected update because it did not improve the function value).
#' * \code{seconds} is the overall computation time in seconds.
#' * \code{stopping_reason} is a message why the procedure has terminated.
#'
#' @examples
#' # Example 1: Minimization of Himmelblau's function --------------------------
#'
#' # see https://en.wikipedia.org/wiki/Himmelblau%27s_function
#' himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
#'
#' ao_fixed(
#'   f = himmelblau,
#'   initial = c(0, 0),              # initialization at the origin
#'   fixed_partition = as.list(1:2), # minimize one parameter cond. on the other
#'   tolerance_value = 1e-6,         # stop if objective value improvem. <= 1e-6
#'   verbose = FALSE
#' )
#'
#' # Example 2: Maximization of 2-class Gaussian mixture log-likelihood --------
#'
#' # targets:
#' # - class means mu (2, unrestricted)
#' # - class standard deviations sd (2, must be non-negative)
#' # - class proportion lambda (only 1 for identification, must be in [0, 1])
#' normal_mixture_llk <- function(mu, sd, lambda, data) {
#'   c1 <- lambda * dnorm(data, mu[1], sd[1])
#'   c2 <- (1 - lambda) * dnorm(data, mu[2], sd[2]))
#'   sum(log(c1 + c2))
#' }
#'
#' # overview of the data set
#' hist(datasets::faithful$eruptions, breaks = 20)
#'
#' # definition of the 'Objective' object
#' objective <- Objective$new(
#'   f = normal_mixture_llk,
#'   target = c("mu", "sd", "lambda"),
#'   npar = c(2, 2, 1),
#'   data = datasets::faithful$eruptions
#' )
#'
#' # definition of the 'Partition' object
#' partition <- Partition$new(npar = 5, type = "random")
#'
#' # definition of the 'Optimizer' object with parameter restriction
#' optimizer <- Optimizer$new(
#'   which = "stats::optim",
#'   lower = -5,
#'   upper = 5,
#'   method = "L-BFGS-B"
#' )
#'
#' # definition of the 'Procedure' object
#' procedure <- Procedure$new(
#'   verbose = TRUE,
#'   minimize = TRUE
#' )
#'
#' # alternating optimization
#' ao(
#'   objective = objective,
#'   partition = partition,
#'   optimizer = optimizer,
#'   initial   = rnorm(5)
#'   procedure = procedure
#' )
#'
#' @export

ao <- function(
    objective,
    partition = Partition$new(npar = sum(objective$npar), type = "random"),
    optimizer = Optimizer$new("stats::optim"),
    initial   = stats::rnorm(sum(objective$npar)),
    procedure = Procedure$new(verbose = TRUE, minimize = TRUE)
  ) {

  ### input checks and preliminary preparations
  ao_input_checks(
    objective = objective, partition = partition, optimizer = optimizer,
    initial = initial, procedure = procedure
  )
  npar <- partition$npar
  block_objective <- ao_build_block_objective(
    partition = partition, objective = objective
  )
  procedure$initialize_details(
    initial_parameter = initial,
    initial_value = objective$evaluate(initial),
    npar = npar
  )

  ### alternating optimization
  while (TRUE) {

    ### check stopping criteria
    if (procedure$check_stopping()) {
      break
    } else {
      procedure$iteration <- procedure$iteration + 1L
    }

    ### generate partition
    next_partition <- partition$get()

    ### optimize over each parameter block in partition
    for (block in next_partition) {
      procedure$block <- block

      ### optimize block objective function
      block_objective_out <- optimizer$optimize(
        objective = block_objective,
        initial = procedure$get_parameter_latest("block"),
        direction = ifelse(procedure$minimize, "min", "max"),
        parameter_fixed = procedure$get_parameter_latest("fixed"),
        block = block
      )

      ### check acceptance and update
      procedure$update_details(
        value = block_objective_out[["value"]],
        parameter_block = block_objective_out[["parameter"]],
        seconds = block_objective_out[["seconds"]],
        error = block_objective_out[["error"]]
      )
    }
  }

  ### return results
  procedure$output
}
