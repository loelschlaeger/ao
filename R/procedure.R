#' Procedure Object
#'
#' @description
#' Details for alternating optimization procedure.
#'
#' @param verbose (`logical(1)`)\cr
#' Whether to print tracing details during the alternating optimization
#' process.
#'
#' @param minimize (`logical(1)`)\cr
#' Whether to minimize during the alternating optimization process.
#' If \code{FALSE}, maximization is performed.
#'
#' @param iteration_limit (`integer(1)` or `Inf`)\cr
#' The maximum number of iterations through the parameter partition before
#' the alternating optimization process is terminated.
#' Can also be `Inf` for no iteration limit.
#'
#' @param seconds_limit (`numeric(1)`)\cr
#' The time limit in seconds before the alternating optimization process is
#' terminated.
#' Can also be `Inf` for no time limit.
#' Note that this stopping criteria is only checked *after*
#' a sub-problem is solved and not *within* solving a sub-problem, so the
#' actual process time can exceed this limit.
#'
#' @param tolerance_value (`numeric(1)`)\cr
#' A non-negative tolerance value. The alternating optimization terminates
#' if the absolute difference between the current function value and the one
#' from the last iteration is smaller than \code{tolerance_value}.
#' Can be `0` for no value threshold.
#'
#' @param tolerance_parameter (`numeric(1)`)\cr
#' A non-negative tolerance value. The alternating optimization terminates
#' if the distance between the current estimate and the one from the last
#' iteration is smaller than \code{tolerance_parameter}.
#' Can be `0` for no parameter threshold.
#' By default, the distance is measured using the euclidean norm, but
#' another norm can be specified via the \code{tolerance_parameter_norm}
#' field.
#'
#' @param tolerance_parameter_norm (`function`)\cr
#' The norm that measures the distance between the current estimate and the
#' one from the last iteration. If the distance is smaller than
#' \code{tolerance_parameter}, the procedure is terminated.
#' It must be of the form \code{function(x, y)} for two vector inputs
#' \code{x} and \code{y}, and return a single \code{numeric} value.
#' By default, the euclidean norm \code{function(x, y) sqrt(sum((x - y)^2))}
#' is used.
#'
#' @param which_iteration (`integer()`)\cr
#' Selects the iteration(s).
#' Can also be \code{NULL} to select all iterations.
#'
#' @param which_block (`character(1)` or `integer()`)\cr
#' Selects the parameter block in the partition and can be one of
#' - \code{"first"} for the first parameter block,
#' - \code{"last"} for the last parameter block,
#' - an `integer` vector of parameter indices,
#' - or \code{NULL} for all parameter blocks.
#'
#' @param which_column (`character()`)\cr
#' Selects the columns in the `details` part of the output and can be one or
#' more of `"iteration"`, `"value"`, `"parameter"`, `"block"`, `"seconds"`,
#' and `"update_code"`.
#'
#' @param keep_iteration_column (`logical()`)\cr
#' Whether to keep the column containing the information about the iteration
#' in the output.
#'
#' @param keep_block_columns (`logical()`)\cr
#' Whether to keep the column containing the information about the active
#' parameter block in the output.
#'
#' @param parameter_type (`character(1)`)\cr
#' Can be one of
#' - \code{"full"} (default) to get the full parameter vector,
#' - \code{"block"} to get the parameter values for the current block,
#'   i.e., the parameters with the indices `self$block`
#' - \code{"fixed"} to get the parameter values which are currently fixed,
#'   i.e., all except for those with the indices `self$block`
#'
#' @export

Procedure <- R6::R6Class("Procedure",
  cloneable = FALSE,
  public = list(

    #' @description
    #' Creates a new object of this [R6][R6::R6Class] class.
    initialize = function(
      verbose = FALSE,
      minimize = TRUE,
      iteration_limit = Inf,
      seconds_limit = Inf,
      tolerance_value = 0,
      tolerance_parameter = 0,
      tolerance_parameter_norm = function(x, y) sqrt(sum((x - y)^2))
    ) {
      self$verbose <- verbose
      self$minimize <- minimize
      self$iteration_limit <- iteration_limit
      self$seconds_limit <- seconds_limit
      self$tolerance_value <- tolerance_value
      self$tolerance_parameter <- tolerance_parameter
      self$tolerance_parameter_norm <- tolerance_parameter_norm
      invisible(self)
    },

    #' @description
    #' Print details about this object.
    print = function() {
      cat("procedure object")
      invisible(self)
    },

    #' @description
    #' Prints a status message.
    #' @param message (`character(1)`)\cr
    #' The status message.
    #' @param message_type (`integer(1)`)\cr
    #' The type of the message, one of the following:
    #' * `1` for `cli::cli_h1()`
    #' * `2` for `cli::cli_h2()`
    #' * `3` for `cli::cli_h3()`
    #' * `4` for `cli::cli_alert_success()`
    #' * `5` for `cli::cli_alert_info()`
    #' * `6` for `cli::cli_alert_warning()`
    #' * `7` for `cli::cli_alert_danger()`
    #' * `8` for `cli::cat_line()`
    status = function(message, message_type = 8, verbose = self$verbose) {
      checkmate::assert_string(message, min.chars = 1)
      checkmate::assert_int(message_type, lower = 1, upper = 8)
      checkmate::assert_flag(verbose)
      if (isTRUE(verbose)) {
        switch(
          message_type,
          cli::cli_h1(message),
          cli::cli_h2(message),
          cli::cli_h3(message),
          cli::cli_alert_success(message),
          cli::cli_alert_info(message),
          cli::cli_alert_warning(message),
          cli::cli_alert_danger(message),
          cli::cat_line(message)
        )
      }
      invisible(self)
    },

    #' @description
    #' Initializes the `details` part of the output.
    #' @param initial_parameter (`numeric()`)\cr
    #' The starting parameter values for the procedure.
    #' @param initial_value (`numeric(1)`)\cr
    #' The function value at the initial parameters.
    #' @param npar (`integer(1)`)\cr
    #' The length of the target argument.
    initialize_details = function(initial_parameter, initial_value, npar) {
      private$.details <- structure(
        data.frame(
          t(c(0L, initial_value, initial_parameter, rep(0, npar), 0, 0L))
        ),
        names = c(
          "iteration", "value", paste0("p", seq_len(npar)),
          paste0("b", seq_len(npar)), "seconds", "update_code"
        )
      )
      private$.npar <- npar
      invisible(self)
    },

    #' @description
    #' Updates the `details` part of the output.
    #' @param value (`numeric(1)`)\cr
    #' The updated function value.
    #' @param parameter_block (`numeric()`)\cr
    #' The updated parameter values for the active parameter block.
    #' @param seconds (`numeric(1)`)\cr
    #' The time in seconds for solving the sub-problem.
    #' @param error (`logical(1)`)\cr
    #' Whether solving the sub-problem resulted in an error.
    #' @param block (`integer()`)\cr
    #' The currently active parameter block, represented as parameter indices.
    update_details = function(
      value, parameter_block, seconds, error, block = self$block
    ) {

      ### check inputs
      check_block <- checkmate::check_integerish(
        block,
        unique = TRUE, lower = 1,
        upper = if (length(private$.npar) == 1) private$.npar else Inf
      )
      check_value <- checkmate::check_number(
        value, na.ok = FALSE, null.ok = FALSE, finite = TRUE
      )
      check_parameter_block <- oeli::check_numeric_vector(
        parameter_block, any.missing = FALSE, len = length(block)
      )
      check_seconds <- checkmate::check_number(
        seconds, na.ok = FALSE, lower = 0, finite = TRUE, null.ok = FALSE
      )
      check_error <- checkmate::check_flag(error)

      ### check whether to accept the update
      check_results <- all(sapply(
        c(check_block, check_value, check_parameter_block, check_seconds, check_error),
        isTRUE
      ))
      update_code <- if (!check_results || error) {
        1
      } else if (self$minimize && any(value >= self$get_value())) {
        2
      } else if (!self$minimize && any(value <= self$get_value())) {
        2
      } else {
        0
      }

      ### update details
      rows <- nrow(private$.details)
      if (update_code == 0) {
        parameter <- self$get_parameter_latest("full")
        parameter[block] <- parameter_block
        private$.details[rows + 1, "iteration"] <- self$iteration
        private$.details[rows + 1, "value"] <- value
        private$.details[rows + 1, "seconds"] <- seconds
        parameter_columns <- which(startsWith(colnames(private$.details), "p"))
        private$.details[rows + 1, parameter_columns] <- parameter
        block_columns <- which(startsWith(colnames(private$.details), "b"))
        private$.details[rows + 1, block_columns[block]] <- 1
        private$.details[rows + 1, block_columns[-block]] <- 0
      } else {
        private$.details[rows + 1, ] <- private$.details[rows, ]
        self$status("update rejected", 6)
      }
      private$.details[rows + 1, "update_code"] <- update_code

      ### return information
      self$status("function value:", 8)
      if (self$verbose) cli::cat_print(value)
      self$status("parameter:", 8)
      if (self$verbose) cli::cat_print(self$get_parameter_latest("full"))
      invisible(self)
    },

    #' @description
    #' Get the `details` part of the output.
    get_details = function(
      which_iteration = NULL, which_block = NULL,
      which_column = c("iteration", "value", "parameter", "block", "seconds", "update_code")
    ) {
      ### input checks
      check <- checkmate::check_integerish(
        which_iteration,
        lower = 0, null.ok = TRUE, min.len = 1, any.missing = FALSE
      )
      if (!isTRUE(check)) {
        cli::cli_abort(
          paste("Input {.var which_iteration} is bad:", check),
          call = NULL
        )
      }
      if (
        !checkmate::test_choice(which_block, c("first", "last"), null.ok = TRUE) &&
          !checkmate::test_integerish(
            which_block,
            lower = 1, upper = private$.npar, unique = TRUE,
            min.len = 1, max.len = private$.npar, any.missing = FALSE
          )
      ) {
        cli::cli_abort(
          "Input {.var which_block} must be one of {.val first}, {.val last},
          {.code NULL}, or of class {.cls integer}",
          call = NULL
        )
      }
      check <- checkmate::check_subset(
        which_column, c("iteration", "value", "parameter", "block", "seconds", "update_code"),
        empty.ok = TRUE
      )
      if (!isTRUE(check)) {
        cli::cli_abort(
          paste("Input {.var which_column} is bad:", check),
          call = NULL
        )
      }

      ### filter details
      details <- private$.details
      if (!is.null(which_iteration)) {
        details <- details[details$iteration %in% which_iteration, ]
      }
      if (!is.null(which_block)) {
        if (is.character(which_block)) {
          rows <- oeli::vector_occurrence(details$iteration, which_block)
        } else {
          rows <- as.integer(which(apply(
            details[, block_columns], 1, function(x) all(as.numeric(x) %in% 1)
          )))
        }
      } else {
        rows <- seq_len(nrow(details))
      }

      ### select columns
      iteration_column <- which(colnames(details) == "iteration")
      value_column <- which(colnames(details) == "value")
      parameter_columns <- which(startsWith(colnames(details), "p"))
      block_columns <- which(startsWith(colnames(details), "b"))
      seconds_column <- which(colnames(details) == "seconds")
      update_code_column <- which(colnames(details) == "update_code")
      columns <- c(
        if ("iteration" %in% which_column) iteration_column,
        if ("value" %in% which_column) value_column,
        if ("parameter" %in% which_column) parameter_columns,
        if ("block" %in% which_column) block_columns,
        if ("seconds" %in% which_column) seconds_column,
        if ("update_code" %in% which_column) update_code_column
      )

      ### return details
      details[rows, columns]
    },

    #' @description
    #' Get the function value in different steps of the alternating optimization
    #' procedure.
    get_value = function(
      which_iteration = NULL,
      which_block = NULL,
      keep_iteration_column = FALSE,
      keep_block_columns = FALSE
    ) {

      ### input checks
      check <- checkmate::test_flag(keep_iteration_column)
      if (!isTRUE(check)) {
        cli::cli_abort(
          paste("Input {.var keep_iteration_column} is bad:", check),
          call = NULL
        )
      }
      check <- checkmate::test_flag(keep_block_columns)
      if (!isTRUE(check)) {
        cli::cli_abort(
          paste("Input {.var keep_block_columns} is bad:", check),
          call = NULL
        )
      }

      ### return values
      self$get_details(
        which_iteration = which_iteration, which_block = which_block,
        which_column = c(
          "value",
          if (keep_iteration_column) "iteration",
          if (keep_block_columns) "block"
        )
      )
    },

    #' @description
    #' Get the function value in the latest step of the alternating
    #' optimization procedure.
    get_value_latest = function() {
      private$.details[nrow(private$.details), "value"]
    },

    #' @description
    #' Get the parameter values in different steps of the alternating
    #' optimization procedure.
    get_parameter = function(
      which_iteration = self$iteration,
      which_block = NULL,
      keep_iteration_column = FALSE,
      keep_block_columns = FALSE
    ) {

      ### input checks
      check <- checkmate::test_flag(keep_iteration_column)
      if (!isTRUE(check)) {
        cli::cli_abort(
          paste("Input {.var keep_iteration_column} is bad:", check),
          call = NULL
        )
      }
      check <- checkmate::test_flag(keep_block_columns)
      if (!isTRUE(check)) {
        cli::cli_abort(
          paste("Input {.var keep_block_columns} is bad:", check),
          call = NULL
        )
      }

      ### get parameters
      self$get_details(
        which_iteration = which_iteration, which_block = which_block,
        which_column = c(
          "parameter",
          if (keep_iteration_column) "iteration",
          if (keep_block_columns) "block"
        )
      )
    },

    #' @description
    #' Get the parameter value in the latest step of the alternating
    #' optimization procedure.
    get_parameter_latest = function(parameter_type = "full") {
      ### input checks
      check <- checkmate::check_choice(parameter_type, c("full", "block", "fixed"))
      if (!isTRUE(check)) {
        cli::cli_abort(
          paste("Input {.var parameter_type} is bad:", check),
          call = NULL
        )
      }

      ### get latest parameter
      details <- private$.details
      parameter_columns <- which(startsWith(colnames(details), "p"))
      latest_parameter <- as.numeric(details[nrow(details), parameter_columns])

      ### filter for 'parameter_type'
      if (parameter_type == "full") {
        return(latest_parameter)
      } else {
        if (parameter_type == "block") {
          return(latest_parameter[self$block])
        } else {
          return(latest_parameter[-self$block])
        }
      }
    },

    #' @description
    #' Get the optimization time in seconds in different steps of the
    #' alternating optimization procedure.
    get_seconds = function(
      which_iteration = NULL,
      which_block = NULL,
      keep_iteration_column = FALSE,
      keep_block_columns = FALSE
    ) {

      ### input checks
      check <- checkmate::test_flag(keep_iteration_column)
      if (!isTRUE(check)) {
        cli::cli_abort(
          paste("Input {.var keep_iteration_column} is bad:", check),
          call = NULL
        )
      }
      check <- checkmate::test_flag(keep_block_columns)
      if (!isTRUE(check)) {
        cli::cli_abort(
          paste("Input {.var keep_block_columns} is bad:", check),
          call = NULL
        )
      }

      ### get parameters
      self$get_details(
        which_iteration = which_iteration, which_block = which_block,
        which_column = c(
          "seconds",
          if (keep_iteration_column) "iteration",
          if (keep_block_columns) "block"
        )
      )
    },

    #' @description
    #' Get the total optimization time in seconds of the alternating
    #' optimization procedure.
    get_seconds_total = function() {
      sum(self$get_seconds(
        which_iteration = NULL, which_block = NULL,
        keep_iteration_column = FALSE, keep_block_columns = FALSE
      ), na.rm = TRUE)
    },

    #' @description
    #' Checks if the alternating optimization procedure can be terminated.
    check_stopping = function(
        iteration_limit = self$iteration_limit,
        seconds_limit = self$seconds_limit,
        tolerance_value = self$tolerance_value,
        tolerance_parameter = self$tolerance_parameter,
        tolerance_parameter_norm = self$tolerance_parameter_norm
      ) {

        ### check inputs
        self$iteration_limit <- iteration_limit
        self$seconds_limit <- seconds_limit
        self$tolerance_value <- tolerance_value
        self$tolerance_parameter <- tolerance_parameter
        self$tolerance_parameter_norm <- tolerance_parameter_norm

        ### check stopping criteria
        stopping <- FALSE
        while (TRUE) {

          ### check iteration limit
          if (self$iteration >= iteration_limit) {
            message <- paste("iteration limit of", iteration_limit, "reached")
            stopping <- TRUE
            break
          }

          ### check time limit
          if (self$get_seconds_total() >= seconds_limit) {
            message <- paste("time limit of", seconds_limit, "seconds reached")
            stopping <- TRUE
            break
          }

          ### only check tolerance if at least one iteration has been performed
          if (self$iteration >= 1) {

            ### check value tolerance
            abs_value_change <- abs(
              self$get_value_latest() - self$get_value(
                which_iteration = self$iteration - 1,
                which_block = "first"
              )
            )
            if (abs_value_change < tolerance_value) {
              message <- paste("change in function value is <", tolerance_value)
              stopping <- TRUE
              break
            }

            ### check parameter tolerance
            parameter_change <- tolerance_parameter_norm(
              self$get_parameter_latest(),
              self$get_parameter(
                which_iteration = self$iteration - 1,
                which_block = "first"
              )
            )
            if (parameter_change < tolerance_parameter) {
              message <- paste("distance of parameters is <", tolerance_parameter)
              stopping <- TRUE
              break
            }
          }
          break
        }

        ### decide for stopping
        if (isTRUE(stopping)) {
          private$.stopping_reason <- message
          self$status("\n", 8)
          self$status(paste("procedure is terminated:", message), 4)
          self$status("\n", 8)
        }
        return(stopping)

      }
  ),
  active = list(

    #' @field verbose (`logical(1)`)\cr
    #' Whether to print tracing details during the alternating optimization
    #' process.
    verbose = function(value) {
      if (missing(value)) {
        private$.verbose
      } else {
        check <- checkmate::check_flag(value)
        if (!isTRUE(check)) {
          cli::cli_abort(
            paste("Input {.var verbose} is bad:", check),
            call = NULL
          )
        }
        private$.verbose <- value
      }
    },

    #' @field minimize (`logical(1)`)\cr
    #' Whether to minimize during the alternating optimization process.
    #' If \code{FALSE}, maximization is performed.
    minimize = function(value) {
      if (missing(value)) {
        private$.minimize
      } else {
        check <- checkmate::check_flag(value)
        if (!isTRUE(check)) {
          cli::cli_abort(
            paste("Input {.var minimize} is bad:", check),
            call = NULL
          )
        }
        private$.minimize <- value
      }
    },

    #' @field iteration_limit (`integer(1)` or `Inf`)\cr
    #' The maximum number of iterations through the parameter partition before
    #' the alternating optimization process is terminated.
    #' Can also be `Inf` for no iteration limit.
    iteration_limit = function(value) {
      if (missing(value)) {
        private$.iteration_limit
      } else {
        check <- checkmate::check_number(value, lower = 1, finite = FALSE)
        if (!isTRUE(check)) {
          cli::cli_abort(
            paste("Input {.var iteration_limit} is bad:", check),
            call = NULL
          )
        }
        if (is.finite(value)) {
          value <- as.integer(value)
        }
        private$.iteration_limit <- value
      }
    },

    #' @field seconds_limit (`numeric(1)`)\cr
    #' The time limit in seconds before the alternating optimization process is
    #' terminated.
    #' Can also be `Inf` for no time limit.
    #' Note that this stopping criteria is only checked *after*
    #' a sub-problem is solved and not *within* solving a sub-problem, so the
    #' actual process time can exceed this limit.
    seconds_limit = function(value) {
      if (missing(value)) {
        private$.seconds_limit
      } else {
        check <- checkmate::check_number(
          value, lower = 0, finite = FALSE, null.ok = FALSE, na.ok = FALSE
        )
        if (!isTRUE(check)) {
          cli::cli_abort(
            paste("Input {.var seconds_limit} is bad:", check),
            call = NULL
          )
        }
        private$.seconds_limit <- value
      }
    },

    #' @field tolerance_value (`numeric(1)`)\cr
    #' A non-negative tolerance value. The alternating optimization terminates
    #' if the absolute difference between the current function value and the one
    #' from the last iteration is smaller than \code{tolerance_value}.
    #' Can be `0` for no value threshold.
    tolerance_value = function(value) {
      if (missing(value)) {
        private$.tolerance_value
      } else {
        check <- checkmate::check_number(value, lower = 0, finite = TRUE)
        if (!isTRUE(check)) {
          cli::cli_abort(
            paste("Input {.var tolerance_value} is bad", check),
            call = NULL
          )
        }
        private$.tolerance_value <- value
      }
    },

    #' @field tolerance_parameter (`numeric(1)`)\cr
    #' A non-negative tolerance value. The alternating optimization terminates
    #' if the distance between the current estimate and the one from the last
    #' iteration is smaller than \code{tolerance_parameter}.
    #' Can be `0` for no parameter threshold.
    #' By default, the distance is measured using the euclidean norm, but
    #' another norm can be specified via the \code{tolerance_parameter_norm}
    #' field.
    tolerance_parameter = function(value) {
      if (missing(value)) {
        private$.tolerance_parameter
      } else {
        check <- checkmate::check_number(value, lower = 0, finite = FALSE)
        if (!isTRUE(check)) {
          cli::cli_abort(
            paste("Input {.var tolerance_parameter} is bad:", check),
            call = NULL
          )
        }
        private$.tolerance_parameter <- value
      }
    },

    #' @field tolerance_parameter_norm (`function`)\cr
    #' The norm that measures the distance between the current estimate and the
    #' one from the last iteration. If the distance is smaller than
    #' \code{tolerance_parameter}, the procedure is terminated.
    #' It must be of the form \code{function(x, y)} for two vector inputs
    #' \code{x} and \code{y}, and return a single \code{numeric} value.
    #' By default, the euclidean norm \code{function(x, y) sqrt(sum((x - y)^2))}
    #' is used.
    tolerance_parameter_norm = function(value) {
      if (missing(value)) {
        private$.tolerance_parameter_norm[[1]]
      } else {
        check <- checkmate::check_function(value, args = c("x", "y"), nargs = 2)
        if (!isTRUE(check)) {
          cli::cli_abort(
            paste("Input {.var tolerance_parameter_norm} is bad:", check),
            call = NULL
          )
        }
        private$.tolerance_parameter_norm <- list(value)
      }
    },

    #' @field iteration (`integer(1)`)\cr
    #' The current iteration number.
    iteration = function(value) {
      if (missing(value)) {
        private$.iteration
      } else {
        check <- checkmate::check_int(value, lower = 0, na.ok = FALSE)
        if (!isTRUE(check)) {
          cli::cli_abort(
            paste("Input {.var iteration} is bad", check),
            call = NULL
          )
        }
        private$.iteration <- as.integer(value)
        self$status(
          paste("iteration", private$.iteration, "of", self$iteration_limit), 2
        )
      }
    },

    #' @field block (`integer()`)\cr
    #' The currently active parameter block, represented as parameter indices.
    block = function(value) {
      if (missing(value)) {
        private$.block
      } else {
        check <- checkmate::check_integerish(
          value,
          unique = TRUE, lower = 1,
          upper = if (length(private$.npar) == 1) private$.npar else Inf
        )
        if (!isTRUE(check)) {
          cli::cli_abort(
            paste("Input {.var block} is bad", check),
            call = NULL
          )
        }
        private$.block <- value
        self$status(
          paste("block [", paste(value, sep = ","), "] : "), 3
        )
      }
    },

    #' @field output (`list()`, read-only)\cr
    #' The output of the alternating optimization procedure, which is a
    #' \code{list} with the following elements:
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
    output = function(value) {
      if (missing(value)) {
        list(
          "estimate" = self$get_parameter_latest("full"),
          "value" = self$get_value_latest(),
          "details" = self$get_details(),
          "seconds" = self$get_seconds_total(),
          "stopping_reason" = private$.stopping_reason
        )
      } else {
        cli::cli_abort(
          "Field {.var output} is read-only",
          call = NULL
        )
      }
    }

  ),
  private = list(
    .verbose = logical(),
    .minimize = logical(),
    .iteration_limit = integer(),
    .seconds_limit = numeric(),
    .tolerance_value = numeric(),
    .tolerance_parameter = numeric(),

    ### must store 'tolerance_parameter_norm' inside list
    .tolerance_parameter_norm = list(function(x, y) sqrt(sum((x - y)^2))),
    .iteration = 0L,
    .block = integer(),
    .details = data.frame(),
    .npar = integer(),
    .stopping_reason = "not terminated yet"
  )
)
