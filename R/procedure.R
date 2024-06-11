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
    initialize = function(
      verbose = FALSE, minimize = TRUE, iteration_limit = 10,
      tolerance_value = 1e-6, tolerance_parameter = 1e-6
    ) {
      self$verbose <- verbose
      self$minimize <- minimize
      self$iteration_limit <- iteration_limit
      self$tolerance_value <- tolerance_value
      self$tolerance_parameter <- tolerance_parameter
      self$iteration <- 0
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
    #' @param iteration
    #' TODO
    get_value = function(iteration = self$iteration) {

    },

    #' @description
    #' TODO
    #' @param type
    #' TODO
    #' @param iteration
    #' TODO
    get_parameter = function(type, iteration = self$iteration) {

    },

    #' @description
    #' TODO
    next_iteration = function(verbose = self$verbose) {
      private$.iteration <- private$.iteration + 1
      self$status(
        paste("iteration", private$.iteration, "of", self$iteration_limit),
        verbose = verbose
      )
      invisible(self)
    },

    #' @description
    #' TODO
    #' @param block
    #' TODO
    next_block = function(block, verbose = self$verbose) {
      self$status(
        paste("- block {", paste(block, sep = ","), "} : "),
        verbose = verbose
      )
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

    #' @field details
    #' TODO
    details = function(value) {
      if (missing(value)) {
        private$.details
      } else {
        cli::cli_abort(
          "{.var details} is read-only",
          call = NULL
        )
      }
    },

    #' @field output
    #' TODO
    output = function(value) {
      self$status("finished alternating optimization")
      list(
        "estimate" = self$get_parameter(),
        "value" = self$get_value(),
        "details" = self$details,
        "seconds" = sum(self$details$seconds, na.rm = TRUE)
      )
    },

    #' @field iteration
    #' TODO
    iteration = function(value) {
      if (missing(value)) {
        private$.iteration
      } else {
        # TODO: check
        private$.iteration <- value
      }
    }

  ),

  private = list(
    .verbose = logical(),
    .minimize = logical(),
    .iteration = integer(),
    .iteration_limit = integer(),
    .tolerance_value = numeric(),
    .tolerance_parameter = numeric(),
    .details = data.frame(),
    .parameter = numeric()
  )
)

