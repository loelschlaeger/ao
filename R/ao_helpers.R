ao_input_checks <- function(
    objective, partition, optimizer, initial, procedure
  ) {
  if (!checkmate::test_class(objective, "Objective")) {
    cli::cli_abort(
      "{.var objective} must be an
      {.help [{.cls Objective}](optimizeR::Objective)} object",
      call = NULL
    )
  }
  if (!checkmate::test_class(partition, "Partition")) {
    cli::cli_abort(
      "{.var partition} must be an
      {.help [{.cls Partition}](ao::Partition)} object",
      call = NULL
    )
  }
  if (sum(objective$npar) != partition$npar) {
    cli::cli_abort(
      "parameter number implied by {.var objective}
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
      "{.var initial} must be a vector of initial values of length
      {.num {partition$npar}}",
      call = NULL
    )
  }
  invisible(TRUE)
}

ao_build_block_objective <- function(

  ) {
  function(block) {
    function(theta_block, theta_rest) {
      theta <- numeric(npar)
      theta[block] <- theta_block
      theta[-block] <- theta_rest
      out <- objective$evaluate(theta)

      # TODO: attributes
      # if ("gradient" %in% names(attributes(out))) {
      #   gradient <- attr(out, "gradient")
      #   if (is.numeric(gradient) && is.vector(gradient)) {
      #     attr(out, "gradient") <- gradient[p_ind]
      #   }
      # }
      # if ("hessian" %in% names(attributes(out))) {
      #   hessian <- attr(out, "hessian")
      #   if (is.numeric(hessian) && is.matrix(hessian)) {
      #     attr(out, "hessian") <- hessian[p_ind, p_ind, drop = FALSE]
      #   }
      # }
      out
    }
  }
}

