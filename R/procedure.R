#' Procedure Object
#'
#' @description
#' Details for alternating optimization procedure.
#'
#' @param verbose (`logical(1)`)\cr
#' Print tracing details during the alternating optimization process?
#'
#' @examples
#' # TODO
#'
#' @export

Procedure <- R6::R6Class("Procedure",
  cloneable = FALSE,
  public = list(

    #' @description
    #' Creates a new object of this [R6][R6::R6Class] class.
    initialize = function(verbose = TRUE, minimize = TRUE, iterations = 10) {

    },

    #' @description
    #' Print details about this object.
    print = function() {

    },

    info = function(message, verbose = self$verbose) {

    },

    next_iteration = function(verbose = self$verbose) {

      self$info(
        paste("iteration", self$iteration, "of", self$iterations),
        verbose = verbose
      )

    },

    next_block = function(block, verbose = self$verbose) {

      self$info(
        paste("- block {", paste(block, sep = ","), "} : "),
        verbose = verbose
      )

    },

    initialize_details = function(initial, value_at_initial, npar) {
      structure(
        data.frame(
          t(c(0L, value, 0, initial, rep(NA, npar)))
        ),
        names = c(
          "iteration", "value", "seconds", paste0("p", seq_len(npar)),
          paste0("b", seq_len(npar))
        )
      )
    },

    update_details = function(
      value, parameter, block
    ) {

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

     #' @field iterations (`integer(1)`)\cr
     #' The maximum number of iterations through the parameter partition before
     #' the alternating optimization process is terminated.
     iterations = function(value) {
       if (missing(value)) {
         private$.iterations
       } else {
         if (!checkmate::test_number(value, lower = 1, finite = FALSE)) {
           cli::cli_abort(
             "{.var iterations} must be an integer greater or equal {.num 1}",
             call = NULL
           )
         } else if (is.finite(value)) {
           value <- as.integer(value)
         }
         private$.iterations <- value
       }
     },

     stopping = function(value) {


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

     details = function(value) {

     },

     output = function(value) {

       self$info("finished alternating optimization")
       list(
         "value" = self$value,
         "estimate" = self$parameter,
         "details" = self$details,
         "seconds" = sum(self$details$seconds, na.rm = TRUE)
       )

     }


  ),

  private = list(

    .verbose = logical(),
    .minimize = logical(),
    .iterations = integer(),

  )

)









if (!checkmate::test_number(tolerance, lower = 0, finite = TRUE)) {
  cli::cli_abort(
    "{.var tolerance} must be a single, non-negative number",
    call = NULL
  )
} else if (tolerance == 0 && identical(iterations, Inf)) {
  cli::cli_abort(
    "{.var tolerance} cannot be {.num 0} if {.var iterations} is {.num Inf}",
    call = NULL
  )
}




