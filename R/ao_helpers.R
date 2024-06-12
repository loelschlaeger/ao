#' Input checks
#'
#' @description
#' This helper function checks the inputs for the \code{\link{ao}} function.
#'
#' @inheritParams ao
#'
#' @return
#' Either throws an error, or invisible \code{TRUE}.
#'
#' @keywords internal

ao_input_checks <- function(
    objective, partition, optimizer, initial, procedure) {
  if (!checkmate::test_class(objective, "Objective")) {
    cli::cli_abort(
      "{.var objective} must be an
      {.help [{.cls Objective}](optimizeR::Objective)} object",
      call = NULL
    )
  }
  if (!checkmate::test_class(partition, "Partition")) {
    cli::cli_abort(
      "{.var partition} must be a
      {.help [{.cls Partition}](ao::Partition)} object",
      call = NULL
    )
  }
  if (sum(objective$npar) != partition$npar) {
    cli::cli_abort(
      "parameter number `npar` implied by {.var objective}
      ({.num {sum(objective$npar)}}) does not match parameter number
      implied by {.var partition} ({.num {partition$npar}})",
      call = NULL
    )
  }
  if (!checkmate::test_class(optimizer, "Optimizer")) {
    cli::cli_abort(
      "{.var optimizer} must be an
      {.help [{.cls Optimizer}](optimizeR::Optimizer)} object",
      call = NULL
    )
  }
  if (!oeli::test_numeric_vector(initial, len = partition$npar)) {
    cli::cli_abort(
      "{.var initial} must be a {.cls numeric} vector of initial values of
      length {.num {partition$npar}}",
      call = NULL
    )
  }
  if (!checkmate::test_class(procedure, "Procedure")) {
    cli::cli_abort(
      "{.var procedure} must be a
      {.help [{.cls Procedure}](ao::Procedure)} object",
      call = NULL
    )
  }
  invisible(TRUE)
}

#' Block objective function
#'
#' @description
#' This helper function builds the block objective function for the alternating
#' optimization procedure.
#'
#' @inheritParams ao
#'
#' @return
#' TODO
#'
#' @keywords internal

ao_build_block_objective <- function(partition) {
  function(parameter_block, parameter_fixed, block) {
    theta <- numeric(partition$npar)
    theta[block] <- parameter_block
    theta[-block] <- parameter_fixed
    out <- objective$evaluate(theta)
    value <- as.numeric(out)
    for (attribute_name in names(partition$block_attributes)) {
      attribute_function <- partition$block_attributes[[attribute_name]]
      attr(value, attribute_name) <- attribute_function(
        x = attr(out, attribute_name), y = block
      )
    }
    return(value)
  }
}
