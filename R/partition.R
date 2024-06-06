#' Partition Object
#'
#' @description
#' Partition of the target argument for alternating optimization.
#'
#' @param npar (`integer(1)`)\cr
#' The length of the target argument.
#'
#' @param type (`character(1)`)\cr
#' The partition type, one of:
#' - `"random"` (random partition in each iteration)
#' - `"fixed"` (partition defined via `$define_fixed_partition()` method)
#' - `"sequential"` (each parameter alone)
#' - `"none"` (no partition, i.e. standard joint optimization)
#'
#' @param fixed_partition (`list()`)\cr
#' Only relevant for `type = "fixed"`.
#' A \code{list} of \code{integer} (vectors), specifying the partition of the
#' target parameter vector.
#'
#' @param parameter_block ()\cr
#' TODO
#'
#' @param closed_form ()\cr
#' TODO
#'
#' @examples
#' # TODO
#'
#' @export

Partition <- R6::R6Class("Partition",
  cloneable = FALSE,
  public = list(

    #' @description
    #' Creates a new object of this [R6][R6::R6Class] class.
    initialize = function(npar, type) {
      ### missing arguments
      if (missing(npar)) {
        cli::cli_abort(
          "please specify {.var npar}",
          call = NULL
        )
      }
      if (missing(type)) {
        cli::cli_abort(
          "please specify {.var type}",
          call = NULL
        )
      }

      ### 'npar' must be defined for object and cannot be changed later on
      checkmate::assert_int(npar, lower = 1)
      private$.npar <- npar

      ### 'type' can be changed later on
      self$type <- type

      ### default arguments
      private$.new_block_probability <- 0.3
      private$.minimum_block_number <- 1

      ### no fixed partition declared
      private$.fixed_partition_definition <- NULL
    },

    #' @description
    #' Print details about this object.
    print = function() {
      cli::cat_line("partition type: ", private$.type)
    },

    #' @description
    #' Return partition.
    get = function(type = self$type) {
      ### return partition depending on 'type'
      switch(type,
        "random"     = private$.random_partition(),
        "fixed"      = private$.fixed_partition(),
        "sequential" = private$.sequential_partition(),
        "none"       = private$.none_partition()
      )
    },

    #' @description
    #' TODO
    define_fixed_partition = function(fixed_partition = list()) {
      ### 'fixed_partition' must be a list, but can be empty
      if (!checkmate::test_list(fixed_partition)) {
        cli::cli_abort(
          "{.var fixed_partition} must be a {.cls list}",
          call = NULL
        )
      }

      ### only parameter indices 1,...,'self$npar' allowed
      if (any(!unlist(fixed_partition) %in% seq_len(self$npar))) {
        cli::cli_abort(
          "{.var fixed_partition} must only contain {.num {seq_len(self$npar)}}",
          call = NULL
        )
      }

      ### set 'fixed_partition'
      private$.fixed_partition_definition <- fixed_partition

      ### return 'self' (invisible) to allow for method chaining
      invisible(self)
    },

    #' @description
    #' TODO
    define_closed_form = function(parameter_block, closed_form) {
      # TODO
    },

    #' @description
    #' Defines how attributes of the objective function are to be used during
    #' the alternating optimization procedure.
    #' @param attribute_name (`character(1)`)\cr
    #' The name of an attribute of the objective output.
    #' @param attribute_definition (`function`)\cr
    #' The definition how the attribute is to be used. The \code{function} must
    #' have the two arguments \code{x} and \code{y}, where \code{x} is the
    #' attribute value and \code{y} the parameter indices in the current block.
    define_block_attribute = function(attribute_name, attribute_definition) {
      if (!checkmate::test_string(attribute_name)) {
        cli::cli_abort(
          "{.var attribute_name} must be a scalar character vector",
          call = NULL
        )
      }
      if (!checkmate::test_function(
        attribute_definition, args = c("x", "y"), ordered = TRUE, nargs = 2
      )) {
        cli::cli_abort(
          "{.var attribute_definition} must be a {.cls function} of the form
          {.code function(x, y)}",
          call = NULL
        )
      }
      private$.block_attributes[[attribute_name]] <- attribute_definition
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
          "{.var npar} is read-only",
          call = NULL
        )
      }
    },

    #' @field type
    #' The partition type, either `"random"`, `"fixed"`, `"sequential"`, or
    #' `"none"`. Supports partial matching.
    type = function(value) {
      if (missing(value)) {
        private$.type
      } else {
        value <- oeli::match_arg(
          arg = value,
          choices = c("random", "fixed", "sequential", "none"),
          several.ok = FALSE,
          none.ok = FALSE
        )
        private$.type <- value
      }
    },

    #' @field new_block_probability (`numeric(1)`)\cr
    #' Only relevant for `type = "random"`.
    #' The probability for a new parameter block in random partitions.
    #' Values close to 0 result in larger parameter blocks, values close to 1
    #' result in smaller parameter blocks.
    new_block_probability = function(value) {
      if (missing(value)) {
        private$.new_block_probability
      } else {
        checkmate::assert_number(
          value,
          lower = 0, upper = 1, .var.name = "new_block_probability"
        )
        private$.new_block_probability <- value
      }
    },

    #' @field minimum_block_number (`integer(1)`)\cr
    #' Only relevant for `type = "random"`.
    #' The minimum number of blocks in random partitions.
    minimum_block_number = function(value) {
      if (missing(value)) {
        private$.minimum_block_number
      } else {
        checkmate::assert_int(
          value,
          lower = 1, upper = self$npar,
          .var.name = "minimum_block_number"
        )
        private$.minimum_block_number <- value
      }
    },

    #' @field block_attributes (`list()`)\cr
    #' TODO
    block_attributes = function(value) {
      if (missing(value)) {
        private$.block_attributes
      } else {
        cli::cli_abort(
          "{.var block_attributes} is read-only",
          call = NULL
        )
      }
    }

  ),
  private = list(
    .npar = integer(),
    .type = character(),
    .fixed_partition_definition = list(),
    .new_block_probability = numeric(),
    .minimum_block_number = integer(),
    .block_attributes = list(),

    #' Generated randomized blocks.
    #' @param x (`integer()`)\cr
    #' The parameter indices.
    #' @param p (`numeric(1)`)\cr
    #' The probability to generate a new block.
    #' @param min (`integer(1)`)\cr
    #' The minimum number of blocks
    #' @author Siddhartha Chib
    .random_partition = function(x = self$npar,
                                 p = self$new_block_probability,
                                 min = self$minimum_block_number) {
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
    },

    #' Return fixed partition.
    .fixed_partition = function() {
      if (is.null(private$.fixed_partition_definition)) {
        cli::cli_abort(
          "fixed partition not yet defined, please call method
          {.fun $define_fixed_partition} first",
          call = NULL
        )
      } else {
        private$.fixed_partition_definition
      }
    },

    # Return sequential partition.
    .sequential_partition = function() {
      as.list(seq_len(private$.npar))
    },

    # Return none partition.
    .none_partition = function() {
      list(seq_len(private$.npar))
    }
  )
)
