#' Input checks
#'
#' @description
#' This helper function checks the inputs for the \code{\link{ao}} function.
#'
#' @param argument_name \[`character(1)`\]\cr
#' The name of the argument that is checked.
#'
#' @param check_result \[`logical(1)`\]\cr
#' \code{TRUE} if the check was successful.
#'
#' @param error_message \[`character(1)`\]\cr
#' An error message to be printed.
#'
#' @param prefix (`character(1)`)\cr
#' A prefix before the error message.
#'
#' @return
#' Either throws an error, or invisible \code{TRUE}.

ao_input_check <- function(
    argument_name, check_result, error_message = check_result,
    prefix = "Input {.var {argument_name}} is bad:") {
  if (!isTRUE(check_result)) {
    cli::cli_abort(paste(prefix, error_message), call = NULL)
  }
  invisible(TRUE)
}

#' Procedure Object
#'
#' @description
#' This object specifies alternating optimization procedure.
#'
#' @param npar (`integer(1)`)\cr
#' The (total) length of the target argument(s).
#'
#' @param partition (`character(1)` or `list()`)\cr
#' Defines the parameter partition, and can be either
#' * `"sequential"` for treating each parameter separately,
#' * `"random"` for a random partition in each iteration,
#' * `"none"` for no partition (which is equivalent to joint optimization),
#' * or a `list` of vectors of parameter indices, specifying a custom
#'   partition for the alternating optimization process.
#'
#' @param new_block_probability (`numeric(1)`)\cr
#' Only relevant if `partition = "random"`.
#' The probability for a new parameter block when creating a random
#' partitions.
#' Values close to 0 result in larger parameter blocks, values close to 1
#' result in smaller parameter blocks.
#'
#' @param minimum_block_number (`integer(1)`)\cr
#' Only relevant if `partition = "random"`.
#' The minimum number of blocks in random partitions.
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
#' before \code{tolerance_history} iterations is smaller than
#' \code{tolerance_value}.
#'
#' Can be `0` for no value threshold.
#'
#' @param tolerance_parameter (`numeric(1)`)\cr
#' A non-negative tolerance value. The alternating optimization terminates if
#' the distance between the current estimate and the before
#' \code{tolerance_history} iterations is smaller than \code{tolerance_parameter}.
#'
#' Can be `0` for no parameter threshold.
#'
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
#' @param tolerance_history (`integer(1)`)\cr
#' The number of iterations to look back to determine whether
#' \code{tolerance_value} or \code{tolerance_parameter} has been reached.
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
#' more of `"iteration"`, `"value"`, `"parameter"`, `"block"`, and `"seconds"`
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

Procedure <- R6::R6Class("Procedure",
   cloneable = FALSE,
   public = list(

   #' @description
   #' Creates a new object of this [R6][R6::R6Class] class.
   initialize = function(
     npar = integer(),
     partition = "sequential",
     new_block_probability = 0.3,
     minimum_block_number = 1,
     verbose = FALSE,
     minimize = TRUE,
     iteration_limit = Inf,
     seconds_limit = Inf,
     tolerance_value = 1e-6,
     tolerance_parameter = 1e-6,
     tolerance_parameter_norm = function(x, y) sqrt(sum((x - y)^2)),
     tolerance_history = 1
    ) {
     ao_input_check(
       "npar",
       checkmate::check_int(npar, lower = 0)
     )
     private$.npar <- npar
     self$partition <- partition
     self$new_block_probability <- new_block_probability
     self$minimum_block_number <- minimum_block_number
     self$verbose <- verbose
     self$minimize <- minimize
     self$iteration_limit <- iteration_limit
     self$seconds_limit <- seconds_limit
     self$tolerance_value <- tolerance_value
     self$tolerance_parameter <- tolerance_parameter
     self$tolerance_parameter_norm <- tolerance_parameter_norm
     self$tolerance_history <- tolerance_history
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
   print_status = function(message, message_type = 8, verbose = self$verbose) {
     checkmate::assert_string(message, min.chars = 1)
     checkmate::assert_int(message_type, lower = 1, upper = 8)
     checkmate::assert_flag(verbose)
     if (isTRUE(verbose)) {
       switch(message_type,
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
   initialize_details = function(initial_parameter, initial_value) {
     private$.details <- structure(
       data.frame(
         t(c(0L, initial_value, initial_parameter, rep(0, self$npar), 0))
       ),
       names = c(
         "iteration", "value", paste0("p", seq_len(self$npar)),
         paste0("b", seq_len(self$npar)), "seconds"
       )
     )
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
   update_details = function(value, parameter_block, seconds, error, block = self$block) {
     ### check inputs
     check_block <- checkmate::check_integerish(
       block,
       unique = TRUE, lower = 1, upper = self$npar
     )
     check_value <- checkmate::check_number(
       value,
       na.ok = FALSE, null.ok = FALSE, finite = TRUE
     )
     check_parameter_block <- oeli::check_numeric_vector(
       parameter_block,
       any.missing = FALSE, len = length(block)
     )
     check_seconds <- checkmate::check_number(
       seconds,
       na.ok = FALSE, lower = 0, finite = TRUE, null.ok = FALSE
     )
     check_error <- checkmate::check_flag(error)
     check_results <- all(sapply(
       c(check_block, check_value, check_parameter_block, check_seconds, check_error),
       isTRUE
     ))

     ### update details
     rows <- nrow(private$.details)
     if (isTRUE(check_results)) {
       parameter <- self$get_parameter_latest("full")
       parameter[block] <- parameter_block
       private$.details[rows + 1, "value"] <- value
       parameter_columns <- which(startsWith(colnames(private$.details), "p"))
       private$.details[rows + 1, parameter_columns] <- parameter
     } else {
       private$.details[rows + 1, ] <- private$.details[rows, ]
     }
     private$.details[rows + 1, "iteration"] <- self$iteration
     private$.details[rows + 1, "seconds"] <- seconds
     block_columns <- which(startsWith(colnames(private$.details), "b"))
     private$.details[rows + 1, block_columns[block]] <- 1
     private$.details[rows + 1, block_columns[-block]] <- 0

     ### return information
     self$print_status("function value:", 8)
     if (self$verbose) cli::cat_print(value)
     self$print_status("parameter:", 8)
     if (self$verbose) cli::cat_print(self$get_parameter_latest("full"))
     invisible(self)
   },

   #' @description
   #' Get a parameter partition.
   get_partition = function() {
     if (checkmate::test_string(self$partition)) {
       switch(self$partition,
              "sequential" = as.list(seq_len(self$npar)),
              "random"     = private$.generate_random_partition(),
              "none"       = list(seq_len(self$npar))
       )
     } else {
       self$partition
     }
   },

   #' @description
   #' Get the `details` part of the output.
   get_details = function(which_iteration = NULL,
                          which_block = NULL,
                          which_column = c("iteration", "value", "parameter", "block", "seconds")) {
     ### input checks
     ao_input_check(
       "which_iteration",
       checkmate::check_integerish(
         which_iteration,
         lower = 0, null.ok = TRUE, min.len = 1, any.missing = FALSE
       )
     )
     ao_input_check(
       "which_block",
       checkmate::test_choice(which_block, c("first", "last"), null.ok = TRUE) ||
         checkmate::test_integerish(
           which_block,
           lower = 1, upper = self$npar, unique = TRUE,
           min.len = 1, max.len = self$npar, any.missing = FALSE
         ),
       "Must be one of {.val first}, {.val last}, {.code NULL}, or of class
       {.cls integer}"
     )
     ao_input_check(
       "which_column",
       checkmate::check_subset(
         which_column, c("iteration", "value", "parameter", "block", "seconds"),
         empty.ok = TRUE
       )
     )

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
     columns <- c(
       if ("iteration" %in% which_column) iteration_column,
       if ("value" %in% which_column) value_column,
       if ("parameter" %in% which_column) parameter_columns,
       if ("block" %in% which_column) block_columns,
       if ("seconds" %in% which_column) seconds_column
     )

     ### return details
     details[rows, columns]
   },

   #' @description
   #' Get the function value in different steps of the alternating optimization
   #' procedure.
   get_value = function(which_iteration = NULL,
                        which_block = NULL,
                        keep_iteration_column = FALSE,
                        keep_block_columns = FALSE) {
     ### input checks
     ao_input_check(
       "keep_iteration_column",
       checkmate::test_flag(keep_iteration_column)
     )
     ao_input_check(
       "keep_block_columns",
       checkmate::test_flag(keep_block_columns)
     )

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
   #' Get the optimum function value in the alternating optimization procedure.
   get_value_best = function() {
     values <- private$.details[, "value"]
     id_best <- if (self$minimize) {
       which.min(values)
     } else {
       which.max(values)
     }
     private$.details[id_best, "value"]
   },

   #' @description
   #' Get the parameter values in different steps of the alternating
   #' optimization procedure.
   get_parameter = function(which_iteration = self$iteration,
                            which_block = NULL,
                            keep_iteration_column = FALSE,
                            keep_block_columns = FALSE) {
     ### input checks
     ao_input_check(
       "keep_iteration_column",
       checkmate::test_flag(keep_iteration_column)
     )
     ao_input_check(
       "keep_block_columns",
       checkmate::test_flag(keep_block_columns)
     )

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
     ao_input_check(
       "parameter_type",
       checkmate::check_choice(parameter_type, c("full", "block", "fixed")),
       "Must be one of {.val full}, {.val block}, or {.val fixed}"
     )

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
   #' Get the optimum parameter value in the alternating optimization procedure.
   get_parameter_best = function(parameter_type = "full") {
     ### input checks
     ao_input_check(
       "parameter_type",
       checkmate::check_choice(parameter_type, c("full", "block", "fixed")),
       "Must be one of {.val full}, {.val block}, or {.val fixed}"
     )

     ### find best parameter
     details <- private$.details
     id_best <- if (self$minimize) {
       which.min(details[, "value"])
     } else {
       which.max(details[, "value"])
     }
     parameter_columns <- which(startsWith(colnames(details), "p"))
     best_parameter <- as.numeric(details[id_best, parameter_columns])

     ### filter for 'parameter_type'
     if (parameter_type == "full") {
       return(best_parameter)
     } else {
       if (parameter_type == "block") {
         return(best_parameter[self$block])
       } else {
         return(best_parameter[-self$block])
       }
     }
   },

   #' @description
   #' Get the optimization time in seconds in different steps of the
   #' alternating optimization procedure.
   get_seconds = function(which_iteration = NULL,
                          which_block = NULL,
                          keep_iteration_column = FALSE,
                          keep_block_columns = FALSE) {
     ### input checks
     ao_input_check(
       "keep_iteration_column",
       checkmate::test_flag(keep_iteration_column)
     )
     ao_input_check(
       "keep_block_columns",
       checkmate::test_flag(keep_block_columns)
     )

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
   check_stopping = function() {
     ### check stopping criteria
     stopping <- FALSE
     while (TRUE) {
       ### check iteration limit
       if (self$iteration >= self$iteration_limit) {
         message <- paste("iteration limit of", self$iteration_limit, "reached")
         stopping <- TRUE
         break
       }

       ### check time limit
       if (self$get_seconds_total() >= self$seconds_limit) {
         message <- paste("time limit of", self$seconds_limit, "seconds reached")
         stopping <- TRUE
         break
       }

       ### only check tolerance if at least 'tolerance_history' iterations have
       ### been performed
       if (self$iteration >= self$tolerance_history) {
         ### check value tolerance
         abs_value_change <- abs(
           self$get_value_latest() - self$get_value(
             which_iteration = self$iteration - self$tolerance_history,
             which_block = "first"
           )
         )
         if (abs_value_change < self$tolerance_value) {
           message <- paste(
             "change in function value between", self$tolerance_history,
             ifelse(self$tolerance_history > 1, "iterations", "iteration"),
             "is <", self$tolerance_value
           )
           stopping <- TRUE
           break
         }

         ### check parameter tolerance
         parameter_change <- self$tolerance_parameter_norm(
           self$get_parameter_latest(),
           self$get_parameter(
             which_iteration = self$iteration - self$tolerance_history,
             which_block = "first"
           )
         )
         if (parameter_change < self$tolerance_parameter) {
           message <- paste(
             "change in function parameters between", self$tolerance_history,
             ifelse(self$tolerance_history > 1, "iterations", "iteration"),
             "is <", self$tolerance_parameter
           )
           stopping <- TRUE
           break
         }
       }
       break
     }

     ### decide for stopping
     if (isTRUE(stopping)) {
       private$.stopping_reason <- message
       self$print_status("\n", 8)
       self$print_status(paste("procedure is terminated:", message), 4)
       self$print_status("\n", 8)
     }
     return(stopping)
   }
 ),
 active = list(

   #' @field npar (`integer(1)`)\cr
   #' The length of the target argument.
   npar = function(value) {
     if (missing(value)) {
       private$.npar
     } else {
       cli::cli_abort(
         "Field {.var npar} is read-only",
         call = NULL
       )
     }
   },

   #' @field partition (`character(1)` or `list()`)\cr
   #' Defines the parameter partition, and can be either
   #' * `"sequential"` for treating each parameter separately,
   #' * `"random"` for a random partition in each iteration,
   #' * `"none"` for no partition (which is equivalent to joint optimization),
   #' * or a `list` of vectors of parameter indices, specifying a custom
   #'   partition for the alternating optimization process.
   partition = function(value) {
     if (missing(value)) {
       private$.partition
     } else {
       if (checkmate::test_string(value)) {
         ao_input_check(
           "partition",
           checkmate::check_choice(value, c("sequential", "random", "none")),
           "Must be one of {.val sequential}, {.val random}, or {.val none}"
         )
       } else if (checkmate::test_list(value)) {
         ao_input_check(
           "partition",
           checkmate::check_list(value)
         )

         ### only parameter indices 1,...,'self$npar' allowed
         ao_input_check(
           "partition",
           checkmate::check_integerish(
             unlist(value),
             lower = 1, upper = self$npar, any.missing = FALSE
           ),
           prefix = "Elements in {.cls list} {.var partition} are bad:"
         )
       } else {
         ao_input_check(
           "partition",
           FALSE,
           "Must be {.val sequential}, {.val random}, {.val none}, or of class {.cls list}"
         )
       }
       private$.partition <- value
     }
   },

   #' @field new_block_probability (`numeric(1)`)\cr
   #' Only relevant if `partition = "random"`.
   #' The probability for a new parameter block when creating a random
   #' partitions.
   #' Values close to 0 result in larger parameter blocks, values close to 1
   #' result in smaller parameter blocks.
   new_block_probability = function(value) {
     if (missing(value)) {
       private$.new_block_probability
     } else {
       ao_input_check(
         "new_block_probability",
         checkmate::check_number(value, lower = 0, upper = 1)
       )
       private$.new_block_probability <- value
     }
   },

   #' @field minimum_block_number (`integer(1)`)\cr
   #' Only relevant if `partition = "random"`.
   #' The minimum number of blocks in random partitions.
   minimum_block_number = function(value) {
     if (missing(value)) {
       private$.minimum_block_number
     } else {
       ao_input_check(
         "minimum_block_number",
         checkmate::check_int(value, lower = 1, upper = self$npar)
       )
       private$.minimum_block_number <- value
     }
   },

   #' @field verbose (`logical(1)`)\cr
   #' Whether to print tracing details during the alternating optimization
   #' process.
   verbose = function(value) {
     if (missing(value)) {
       private$.verbose
     } else {
       ao_input_check(
         "verbose",
         checkmate::check_flag(value)
       )
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
       ao_input_check(
         "minimize",
         checkmate::check_flag(value)
       )
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
       ao_input_check(
         "iteration_limit",
         checkmate::check_number(value, lower = 1, finite = FALSE)
       )
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
       ao_input_check(
         "seconds_limit",
         checkmate::check_number(
           value,
           lower = 0, finite = FALSE, null.ok = FALSE, na.ok = FALSE
         )
       )
       private$.seconds_limit <- value
     }
   },

   #' @field tolerance_value (`numeric(1)`)\cr
   #' A non-negative tolerance value. The alternating optimization terminates
   #' if the absolute difference between the current function value and the one
   #' before \code{tolerance_history} iterations is smaller than
   #' \code{tolerance_value}.
   #'
   #' Can be `0` for no value threshold.
   tolerance_value = function(value) {
     if (missing(value)) {
       private$.tolerance_value
     } else {
       ao_input_check(
         "tolerance_value",
         checkmate::check_number(value, lower = 0, finite = TRUE)
       )
       private$.tolerance_value <- value
     }
   },

   #' @field tolerance_parameter (`numeric(1)`)\cr
   #' A non-negative tolerance value. The alternating optimization terminates if
   #' the distance between the current estimate and the before
   #' \code{tolerance_history} iterations is smaller than \code{tolerance_parameter}.
   #'
   #' Can be `0` for no parameter threshold.
   #'
   #' By default, the distance is measured using the euclidean norm, but
   #' another norm can be specified via the \code{tolerance_parameter_norm}
   #' field.
   tolerance_parameter = function(value) {
     if (missing(value)) {
       private$.tolerance_parameter
     } else {
       ao_input_check(
         "tolerance_parameter",
         checkmate::check_number(value, lower = 0, finite = FALSE)
       )
       private$.tolerance_parameter <- value
     }
   },

   #' @field tolerance_parameter_norm (`function`)\cr
   #' The norm that measures the distance between the current estimate and the
   #' one from the last iteration. If the distance is smaller than
   #' \code{tolerance_parameter}, the procedure is terminated.
   #'
   #' It must be of the form \code{function(x, y)} for two vector inputs
   #' \code{x} and \code{y}, and return a single \code{numeric} value.
   #' By default, the euclidean norm \code{function(x, y) sqrt(sum((x - y)^2))}
   #' is used.
   tolerance_parameter_norm = function(value) {
     if (missing(value)) {
       private$.tolerance_parameter_norm[[1]]
     } else {
       ao_input_check(
         "tolerance_parameter_norm",
         checkmate::check_function(value, args = c("x", "y"), nargs = 2)
       )
       private$.tolerance_parameter_norm <- list(value)
     }
   },

   #' @field tolerance_history (`integer(1)`)\cr
   #' The number of iterations to look back to determine whether
   #' \code{tolerance_value} or \code{tolerance_parameter} has been reached.
   tolerance_history = function(value) {
     if (missing(value)) {
       private$.tolerance_history
     } else {
       ao_input_check(
         "tolerance_history",
         checkmate::check_count(value, positive = TRUE)
       )
       private$.tolerance_history <- value
     }
   },

   #' @field iteration (`integer(1)`)\cr
   #' The current iteration number.
   iteration = function(value) {
     if (missing(value)) {
       private$.iteration
     } else {
       ao_input_check(
         "iteration",
         checkmate::check_int(value, lower = 0, na.ok = FALSE)
       )
       private$.iteration <- as.integer(value)
       self$print_status(
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
       ao_input_check(
         "block",
         checkmate::check_integerish(
           value,
           unique = TRUE, lower = 1, upper = self$npar
         )
       )
       private$.block <- value
       self$print_status(
         paste("block [", paste(value, collapse = ","), "] : "), 3
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
   #'   in the active parameter block and `0` if not), and computation times in seconds
   #'   (column `seconds`)
   #' * \code{seconds} is the overall computation time in seconds.
   #' * \code{stopping_reason} is a message why the procedure has terminated.
   output = function(value) {
     if (missing(value)) {
       list(
         "estimate" = self$get_parameter_best("full"),
         "value" = self$get_value_best(),
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

   ### inputs
   .npar = integer(),
   .partition = NULL,
   .new_block_probability = numeric(),
   .minimum_block_number = integer(),
   .verbose = logical(),
   .minimize = logical(),
   .iteration_limit = integer(),
   .seconds_limit = numeric(),
   .tolerance_value = numeric(),
   .tolerance_parameter = numeric(),
   .tolerance_history = integer(),

   ### must store 'tolerance_parameter_norm' inside list
   .tolerance_parameter_norm = list(function(x, y) sqrt(sum((x - y)^2))),

   ### results
   .iteration = 0L,
   .block = integer(),
   .details = data.frame(),
   .stopping_reason = "not terminated yet",

   # Generated randomized blocks.
   # @param x (`integer()`)\cr
   # The parameter indices.
   # @param p (`numeric(1)`)\cr
   # The probability to generate a new block.
   # @param min (`integer(1)`)\cr
   # The minimum number of blocks
   # @author Siddhartha Chib
   .generate_random_partition = function(
      x = self$npar,
      p = self$new_block_probability,
      min = self$minimum_block_number
    ) {
     if (min == x) {
       return(as.list(seq_len(x)))
     }
     x <- sample(x, replace = F)
     n <- length(x)
     y <- sample(0:1, n, replace = T, prob = c(1 - p, p))
     y[1] <- 1
     ind <- which(y %in% 1)
     if (length(ind) < min) {
       ind <- sort(c(ind, sample(which(y == 0), size = min - length(ind))))
     }
     B <- length(ind)
     blocks <- vector("list", B)
     for (j in seq_len(B)) {
       s <- ind[j]
       if (j < B) {
         e <- ind[(j + 1)] - 1
       } else {
         e <- n
       }
       xj <- x[s:e]
       blocks[[j]] <- xj[order(xj)]
     }
     blocks
   }
 )
)
