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
    #' @param parameter
    #' TODO
    #' @param block
    #' TODO
    update_details = function(value, block, parameter, seconds) {
      ### TODO: accept update?

      seconds <- block_objective_out[["seconds"]]
      value_new <- block_objective_out[["value"]]
      if (checkmate::test_number(value_new, finite = TRUE)) {
        value <- value_new
        est[block] <- block_objective_out[["parameter"]]
      }
      sequence[nrow(sequence) + 1, ] <-
        c(iteration, value, seconds, est, seq_len(npar) %in% block)
      if (verbose) {
        cat("value =", value, "\n")
      }
      invisible(self)
    },

    #' @description
    #' TODO
    get_details = function(which_iteration = self$iteration, which_block = NULL,
                           which_column = c("iteration", "value", "seconds", "parameter", "block")) {
      ### input checks
      if (
        !checkmate::test_integerish(
          which_iteration,
          lower = 0, null.ok = TRUE, min.len = 1,
          any.missing = FALSE
        )
      ) {
        cli::cli_abort(
          "{.var which_iteration} must be non-negative integers or {.code NULL}",
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
          "{.var which_block} must be one of {.val first}, {.val last},
          {.code NULL}, or of class {.cls integer}.",
          call = NULL
        )
      }
      if (!checkmate::test_subset(
        which_column, c("iteration", "value", "seconds", "parameter", "block"),
        empty.ok = TRUE
      )) {
        cli::cli_abort(
          "{.var which_column} must be a subset of {.val iteration},
          {.val value}, {.val seconds}, {.val parameter}, and {.val block}.",
          call = NULL
        )
      }

      ### filter details
      details <- private$.details
      if (!is.null(which_iteration)) {
        details <- details[details$iteration %in% iteration, ]
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
    #' @return
    #' TODO
    get_value = function(which_iteration = self$iteration, which_block = NULL,
                         keep_iteration_column = FALSE, keep_block_columns = FALSE) {
      ### input checks
      if (!checkmate::test_flag(keep_iteration_column)) {
        cli::cli_abort(
          "{.var keep_iteration_column} must be {.code TRUE} or {.code FALSE}",
          call = NULL
        )
      }
      if (!checkmate::test_flag(keep_block_columns)) {
        cli::cli_abort(
          "{.var keep_block_columns} must be {.code TRUE} or {.code FALSE}",
          call = NULL
        )
      }

      ### return values
      self$details(
        which_iteration = which_iteration, which_block = which_block,
        which_column = c(
          "value",
          if (keep_iteration_column) "iteration",
          if (keep_block_columns) "block"
        )
      )
    },

    #' @description
    #' Get the parameter values in different steps of the alternating
    #' optimization procedure.
    #' @param parameter_type (`character(1)`)\cr
    #' Can be one of
    #' - \code{"full"} (default) to get the full parameter vector,
    #' - \code{"block"} to get the parameter values for the current block,
    #'   i.e., the parameters with the indices `self$block`
    #' - \code{"fixed"} to get the parameter values which are currently fixed,
    #'   i.e., all except for those with the indices `self$block`
    #' @return
    #' TODO
    get_parameter = function(parameter_type = "full", which_iteration = self$iteration,
                             which_block = NULL, keep_iteration_column = FALSE,
                             keep_block_columns = FALSE) {
      ### input checks
      if (!checkmate::test_choice(parameter_type, c("full", "block", "fixed"))) {
        cli::cli_abort(
          "{.var parameter_type} must be one of {.val full}, {.val block},
          or {.val fixed}.",
          call = NULL
        )
      }
      if (!checkmate::test_flag(keep_iteration_column)) {
        cli::cli_abort(
          "{.var keep_iteration_column} must be {.code TRUE} or {.code FALSE}",
          call = NULL
        )
      }
      if (!checkmate::test_flag(keep_block_columns)) {
        cli::cli_abort(
          "{.var keep_block_columns} must be {.code TRUE} or {.code FALSE}",
          call = NULL
        )
      }

      ### get parameters
      details <- self$details(
        which_iteration = which_iteration, which_block = which_block,
        which_column = c(
          "parameter",
          if (keep_iteration_column) "iteration",
          if (keep_block_columns) "block"
        )
      )

      ### filter type
      parameter_columns <- which(startsWith(colnames(details), "p"))
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
        if (!checkmate::test_flag(value)) {
          cli::cli_abort(
            "{.var verbose} must be TRUE or FALSE",
            call = NULL
          )
        }
        private$.verbose <- value
      }
    },

    #' @field minimize (`logical(1)`)\cr
    #' Whether to minimize during the alternating optimization process.
    #' Alternatively, maximization is performed.
    minimize = function(value) {
      if (missing(value)) {
        private$.minimize
      } else {
        if (!checkmate::test_flag(value)) {
          cli::cli_abort(
            "{.var minimize} must be TRUE or FALSE",
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
        if (!checkmate::test_number(value, lower = 1, finite = FALSE)) {
          cli::cli_abort(
            "{.var iteration_limit} must be an integer greater or equal {.num 1}",
            call = NULL
          )
        } else if (is.finite(value)) {
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
        if (!checkmate::test_number(value, lower = 0, finite = TRUE)) {
          cli::cli_abort(
            "{.var tolerance_value} must be a number greater or equal {.num 0}",
            call = NULL
          )
        }
        private$.tolerance_value <- value
      }
    },

    #' @field tolerance_parameter (`integer(1)`)\cr
    #' A non-negative tolerance value. The alternating optimization terminates
    #' prematurely (i.e., before \code{iteration_limit} is reached) if the
    #' euclidean distance between the current estimate and the one from the last
    #' iteration is smaller than \code{tolerance_parameter}.
    tolerance_parameter = function(value) {
      if (missing(value)) {
        private$.tolerance_parameter
      } else {
        if (!checkmate::test_number(value, lower = 0, finite = FALSE)) {
          cli::cli_abort(
            "{.var tolerance_parameter} must be an integer greater or equal {.num 0}",
            call = NULL
          )
        }
        private$.tolerance_parameter <- value
      }
    },

    #' @field stopping
    #' TODO
    stopping = function(value) {
      if (private$.iteration == 0) {
        return(FALSE)
      }
      latest_parameter <- unlist(sequence[nrow(sequence), parameter_columns])
      dist <- sqrt(sum(current_parameter - latest_parameter)^2)
      if (dist < tolerance) {
        exit_flag <- TRUE
        if (verbose) {
          cat("tolerance reached : distance =", dist, "<", tolerance, "\n")
        }
        break
      } else {
        iteration <- iteration + 1
      }
    },

    #' @field output
    #' TODO
    output = function(value) {
      if (missing(value)) {
        list(
          "estimate" = self$get_parameter(),
          "value" = self$get_value(),
          "details" = self$get_details(),
          "seconds" = sum(self$get_seconds(), na.rm = TRUE)
        )
      } else {
        cli::cli_abort(
          "{.var output} is read-only",
          call = NULL
        )
      }
    },

    #' @field iteration
    #' TODO
    iteration = function(value) {
      if (missing(value)) {
        private$.iteration
      } else {
        # TODO: check
        private$.iteration <- value
        self$status(
          paste("iteration", private$.iteration, "of", self$iteration_limit)
        )
      }
    },

    #' @field block
    #' TODO
    block = function(value) {
      if (missing(value)) {
        private$.block
      } else {
        # TODO: check
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
    .iteration = 0L,
    .block = integer(),
    .details = data.frame(),
    .npar = integer()
  )
)
