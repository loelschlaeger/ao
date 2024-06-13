#' Procedure Object
#'
#' @description
#' Details for alternating optimization procedure.
#'
#' @param verbose (`logical(1)`)\cr
#' Print tracing details during the alternating optimization process?
#'
#' @param minimize (`logical(1)`)\cr
#' Perform minimization? Alternatively, maximization is performed.
#'
#' @param iteration_limit (`integer(1)`)\cr
#' The maximum number of iterations through the parameter partition before the
#' alternating optimization process is terminated. Can also be \code{Inf}.
#'
#' @param tolerance_value (`numeric(1)`)\cr
#' A non-negative tolerance value. The alternating optimization terminates
#' prematurely (i.e., before \code{iteration_limit} is reached) if the absolute
#' difference between the current function value and the one from the last
#' iteration is smaller than \code{tolerance_value}.
#'
#' @param tolerance_parameter (`numeric(1)`)\cr
#' A non-negative tolerance value. The alternating optimization terminates
#' prematurely (i.e., before \code{iteration_limit} is reached) if the euclidean
#' distance between the current estimate and the one from the last iteration is
#' smaller than \code{tolerance_parameter}.
#'
#' @param parameter_type (`character(1)`)\cr
#' Can be one of
#' - \code{"full"} (default) to get the full parameter vector,
#' - \code{"block"} to get the parameter values for the current block,
#'   i.e., the parameters with the indices `self$block`
#' - \code{"fixed"} to get the parameter values which are currently fixed,
#'   i.e., all except for those with the indices `self$block`
#'
#' @examples
#' Procedure$new(
#'   verbose = FALSE,
#'   minimize = TRUE,
#'   iteration_limit = 10,
#'   tolerance_value = 1e-6,
#'   tolerance_parameter = 1e-6
#' )
#'
#' @export

Procedure <- R6::R6Class("Procedure",
  cloneable = FALSE,
  public = list(

    #' @description
    #' Creates a new object of this [R6][R6::R6Class] class.
    initialize = function(verbose = FALSE, minimize = TRUE, iteration_limit = 10,
                          tolerance_value = 1e-6, tolerance_parameter = 1e-6) {
      self$verbose <- verbose
      self$minimize <- minimize
      self$iteration_limit <- iteration_limit
      self$tolerance_value <- tolerance_value
      self$tolerance_parameter <- tolerance_parameter
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
    status = function(message, verbose = self$verbose) {
      if (isTRUE(verbose)) {
        cat(message, "\n")
      }
      invisible(self)
    },

    #' @description
    #' TODO
    #' @param initial
    #' TODO
    #' @param value
    #' TODO
    #' @param npar
    #' TODO
    initialize_details = function(initial, value, npar) {
      private$.details <- structure(
        data.frame(
          t(c(0L, value, 0, initial, rep(NA, npar)))
        ),
        names = c(
          "iteration", "value", "seconds", paste0("p", seq_len(npar)),
          paste0("b", seq_len(npar))
        )
      )
      private$.npar <- npar
      invisible(self)
    },

    #' @description
    #' TODO
    #' @param value
    #' TODO
    #' @param parameter_block
    #' TODO
    #' @param seconds
    #' TODO
    #' @param block
    #' TODO
    update_details = function(
      value, parameter_block, seconds, block = self$block
    ) {

      ### check whether to accept the update
      accept <- if (procedure$minimize) {
        all(value < self$get_value())
      } else {
        all(value > self$get_value())
      }
      if (isFALSE(accept)) {
        # TODO
        warning("do not accept")
      }

      ### update details
      parameter <- self$get_parameter_latest("full")
      parameter[block] <- parameter_block
      rows <- nrow(private$.details)
      private$.details[rows + 1, "iteration"] <- self$iteration
      private$.details[rows + 1, "value"] <- value
      private$.details[rows + 1, "seconds"] <- seconds
      parameter_columns <- which(startsWith(colnames(private$.details), "p"))
      private$.details[rows + 1, parameter_columns] <- parameter
      block_columns <- which(startsWith(colnames(private$.details), "b"))
      private$.details[rows + 1, block_columns[block]] <- 1
      private$.details[rows + 1, block_columns[-block]] <- 0
      self$status(paste("value =", value))
      invisible(self)
    },

    #' @description
    #' TODO
    get_details = function(
      which_iteration = NULL, which_block = NULL,
      which_column = c("iteration", "value", "seconds", "parameter", "block")
    ) {

      ### input checks
      check <- checkmate::check_integerish(
        which_iteration, lower = 0, null.ok = TRUE, min.len = 1, any.missing = FALSE
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
        which_column, c("iteration", "value", "seconds", "parameter", "block"),
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
      seconds_column <- which(colnames(details) == "seconds")
      parameter_columns <- which(startsWith(colnames(details), "p"))
      block_columns <- which(startsWith(colnames(details), "b"))
      columns <- c(
        if ("iteration" %in% which_column) iteration_column,
        if ("value" %in% which_column) value_column,
        if ("seconds" %in% which_column) seconds_column,
        if ("parameter" %in% which_column) parameter_columns,
        if ("block" %in% which_column) block_columns
      )

      ### return details
      details[rows, columns]
    },

    #' @description
    #' Get the function value in different steps of the alternating optimization
    #' procedure.
    #' @param which_iteration (`integer()`)\cr
    #' Selects the iteration(s).
    #' Can also be \code{NULL} to select all iterations.
    #' @param which_block (`character(1)` or `integer()`)\cr
    #' Selects the parameter block in the partition and can be one of
    #' - \code{"first"} for the first parameter block,
    #' - \code{"last"} for the last parameter block,
    #' - an `integer` vector of parameter indices,
    #' - or \code{NULL} for all parameter blocks.
    get_value = function(
      which_iteration = NULL, which_block = NULL,
      keep_iteration_column = FALSE, keep_block_columns = FALSE
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
        which_iteration = self$iteration, which_block = NULL,
        keep_iteration_column = FALSE, keep_block_columns = FALSE
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
    #' TODO
    get_seconds = function(
      which_iteration = self$iteration, which_block = NULL,
      keep_iteration_column = FALSE, keep_block_columns = FALSE
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
    #' Checks if the alternating optimization procedure can be terminated.
    check_stopping = function(
      iteration_limit = self$iteration_limit,
      tolerance_value = self$tolerance_value,
      tolerance_parameter = self$tolerance_parameter,
      tolerance_parameter_norm = self$tolerance_parameter_norm,
      verbose = self$verbose
    ) {
      if (self$iteration == 0) {
        return(FALSE)
      }
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

    #' @field iteration_limit (`integer(1)`)\cr
    #' The maximum number of iterations through the parameter partition before
    #' the alternating optimization process is terminated.
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

    #' @field tolerance_value (`numeric(1)`)\cr
    #' A non-negative tolerance value. The alternating optimization terminates
    #' prematurely (i.e., before \code{iteration_limit} is reached) if the
    #' absolute difference between the current function value and the one from
    #' the last iteration is smaller than \code{tolerance_value}.
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

    #' @field tolerance_parameter (`integer(1)`)\cr
    #' A non-negative tolerance value. The alternating optimization terminates
    #' prematurely (i.e., before \code{iteration_limit} is reached) if the
    #' distance between the current estimate and the one from the last
    #' iteration is smaller than \code{tolerance_parameter}.
    #' By default, the distance is measured with the euclidean norm, but the
    #' norm can be specified via the \code{tolerance_parameter_norm} field.
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
        private$.tolerance_parameter
      } else {
        check <- checkmate::check_function(value, args = c("x", "y"), nargs = 2)
        if (!isTRUE(check)) {
          cli::cli_abort(
            paste("{.var tolerance_parameter_norm} is bad:", check),
            call = NULL
          )
        }
        private$.tolerance_parameter_norm <- value
      }
    },

    #' @field output (`list()`)\cr
    #' The results of the alternating optimization procedure, contains
    #' * \code{"estimate"}, the parameter vector at termination,
    #' * \code{"value"}, the function value at termination,
    #' * \code{"details"}, a \code{data.frame} of the function values,
    #'   parameters and computation times in the single iterations,
    #' * \code{"seconds"}, the overall computation time in seconds.
    output = function(value) {
      if (missing(value)) {
        list(
          "estimate" = self$get_parameter_latest("full"),
          "value" = self$get_value_latest(),
          "details" = self$get_details(),
          "seconds" = sum(self$get_seconds(), na.rm = TRUE)
        )
      } else {
        cli::cli_abort(
          "Field {.var output} is read-only",
          call = NULL
        )
      }
    },

    #' @field iteration (`integer(1)`)\cr
    #' The current iteration number.
    iteration = function(value) {
      if (missing(value)) {
        private$.iteration
      } else {
        check <- checkmate::check_int(value, lower = 0)
        if (!isTRUE(check)) {
          cli::cli_abort(
            paste("Input {.var iteration} is bad", check),
            call = NULL
          )
        }
        private$.iteration <- as.integer(value)
        self$status(
          paste("iteration", private$.iteration, "of", self$iteration_limit)
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
          value, unique = TRUE, lower = 1,
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
          paste("- block {", paste(value, sep = ","), "} : ")
        )
      }
    }

  ),
  private = list(
    .verbose = logical(),
    .minimize = logical(),
    .iteration_limit = integer(),
    .tolerance_value = numeric(),
    .tolerance_parameter = numeric(),
    .tolerance_parameter_norm = function(x, y) sqrt(sum((x - y)^2)),
    .iteration = 0L,
    .block = integer(),
    .details = data.frame(),
    .npar = integer()
  )
)
