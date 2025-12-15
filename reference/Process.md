# Process Object

This object specifies an AO process.

## Active bindings

- `npar`:

  \[`integer(1)`\]  
  The (total) length of the target argument(s).

- `partition`:

  \[`character(1)` \| [`list()`](https://rdrr.io/r/base/list.html)\]  
  Defines the parameter partition, and can be either

  - `"sequential"` for treating each parameter separately,

  - `"random"` for a random partition in each iteration,

  - `"none"` for no partition (which is equivalent to joint
    optimization),

  - or a `list` of vectors of parameter indices, specifying a custom
    partition for the AO process.

- `new_block_probability`:

  \[`numeric(1)`\]  
  Only relevant if `partition = "random"`.

  The probability for a new parameter block when creating a random
  partition.

  Values close to 0 result in larger parameter blocks, values close to 1
  result in smaller parameter blocks.

- `minimum_block_number`:

  \[`integer(1)`\]  
  Only relevant if `partition = "random"`.

  The minimum number of blocks in random partitions.

- `verbose`:

  \[`logical(1)`\]  
  Print tracing details during the AO process?

- `minimize`:

  \[`logical(1)`\]  
  Minimize during the AO process?

  If `FALSE`, maximization is performed.

- `iteration_limit`:

  \[`integer(1)` \| `Inf`\]  
  The maximum number of iterations through the parameter partition
  before the AO process is terminated.

  Can also be `Inf` for no iteration limit.

- `seconds_limit`:

  \[`numeric(1)`\]  
  The time limit in seconds before the AO process is terminated.

  Can also be `Inf` for no time limit.

  Note that this stopping criteria is only checked *after* a sub-problem
  is solved and not *within* solving a sub-problem, so the actual
  process time can exceed this limit.

- `tolerance_value`:

  \[`numeric(1)`\]  
  A non-negative tolerance value. The AO process terminates if the
  absolute difference between the current function value and the one
  before `tolerance_history` iterations is smaller than
  `tolerance_value`.

  Can be `0` for no value threshold.

- `tolerance_parameter`:

  \[`numeric(1)`\]  
  A non-negative tolerance value. The AO process terminates if the
  distance between the current estimate and the before
  `tolerance_history` iterations is smaller than `tolerance_parameter`.

  Can be `0` for no parameter threshold.

  By default, the distance is measured using the euclidean norm, but
  another norm can be specified via the `tolerance_parameter_norm`
  field.

- `tolerance_parameter_norm`:

  \[`function`\]  
  The norm that measures the distance between the current estimate and
  the one from the last iteration. If the distance is smaller than
  `tolerance_parameter`, the AO process is terminated.

  It must be of the form `function(x, y)` for two vector inputs `x` and
  `y`, and return a single `numeric` value. By default, the euclidean
  norm `function(x, y) sqrt(sum((x - y)^2))` is used.

- `tolerance_history`:

  \[`integer(1)`\]  
  The number of iterations to look back to determine whether
  `tolerance_value` or `tolerance_parameter` has been reached.

- `add_details`:

  \[`logical(1)`\]  
  Add details about the AO process to the output?

- `iteration`:

  \[`integer(1)`\]  
  The current iteration number.

- `block`:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  The currently active parameter block, represented as parameter
  indices.

- `output`:

  \[[`list()`](https://rdrr.io/r/base/list.html), read-only\]  
  The output of the AO process, which is a `list` with the following
  elements:

  - `estimate` is the parameter vector at termination.

  - `value` is the function value at termination.

  - `details` is a `data.frame` with full information about the AO
    process. For each iteration (column `iteration`) it contains the
    function value (column `value`), parameter values (columns starting
    with `p` followed by the parameter index), the active parameter
    block (columns starting with `b` followed by the parameter index,
    where `1` stands for a parameter contained in the active parameter
    block and `0` if not), and computation times in seconds (column
    `seconds`). Only available if `add_details = TRUE`.

  - `seconds` is the overall computation time in seconds.

  - `stopping_reason` is a message why the AO process has terminated.

## Methods

### Public methods

- [`Process$new()`](#method-Process-new)

- [`Process$print_status()`](#method-Process-print_status)

- [`Process$initialize_details()`](#method-Process-initialize_details)

- [`Process$update_details()`](#method-Process-update_details)

- [`Process$get_partition()`](#method-Process-get_partition)

- [`Process$get_details()`](#method-Process-get_details)

- [`Process$get_value()`](#method-Process-get_value)

- [`Process$get_value_latest()`](#method-Process-get_value_latest)

- [`Process$get_value_best()`](#method-Process-get_value_best)

- [`Process$get_parameter()`](#method-Process-get_parameter)

- [`Process$get_parameter_latest()`](#method-Process-get_parameter_latest)

- [`Process$get_parameter_best()`](#method-Process-get_parameter_best)

- [`Process$get_seconds()`](#method-Process-get_seconds)

- [`Process$get_seconds_total()`](#method-Process-get_seconds_total)

- [`Process$check_stopping()`](#method-Process-check_stopping)

------------------------------------------------------------------------

### Method `new()`

Creates a new object of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    Process$new(
      npar = integer(),
      partition = "sequential",
      new_block_probability = 0.3,
      minimum_block_number = 1,
      verbose = FALSE,
      minimize = TRUE,
      iteration_limit = Inf,
      seconds_limit = Inf,
      tolerance_value = 1e-06,
      tolerance_parameter = 1e-06,
      tolerance_parameter_norm = function(x, y) sqrt(sum((x - y)^2)),
      tolerance_history = 1,
      add_details = TRUE
    )

#### Arguments

- `npar`:

  \[`integer(1)`\]  
  The (total) length of the target argument(s).

- `partition`:

  \[`character(1)` \| [`list()`](https://rdrr.io/r/base/list.html)\]  
  Defines the parameter partition, and can be either

  - `"sequential"` for treating each parameter separately,

  - `"random"` for a random partition in each iteration,

  - `"none"` for no partition (which is equivalent to joint
    optimization),

  - or a `list` of vectors of parameter indices, specifying a custom
    partition for the AO process.

- `new_block_probability`:

  \[`numeric(1)`\]  
  Only relevant if `partition = "random"`.

  The probability for a new parameter block when creating a random
  partition.

  Values close to 0 result in larger parameter blocks, values close to 1
  result in smaller parameter blocks.

- `minimum_block_number`:

  \[`integer(1)`\]  
  Only relevant if `partition = "random"`.

  The minimum number of blocks in random partitions.

- `verbose`:

  \[`logical(1)`\]  
  Print tracing details during the AO process?

- `minimize`:

  \[`logical(1)`\]  
  Minimize during the AO process?

  If `FALSE`, maximization is performed.

- `iteration_limit`:

  \[`integer(1)` \| `Inf`\]  
  The maximum number of iterations through the parameter partition
  before the AO process is terminated.

  Can also be `Inf` for no iteration limit.

- `seconds_limit`:

  \[`numeric(1)`\]  
  The time limit in seconds before the AO process is terminated.

  Can also be `Inf` for no time limit.

  Note that this stopping criteria is only checked *after* a sub-problem
  is solved and not *within* solving a sub-problem, so the actual
  process time can exceed this limit.

- `tolerance_value`:

  \[`numeric(1)`\]  
  A non-negative tolerance value. The AO process terminates if the
  absolute difference between the current function value and the one
  before `tolerance_history` iterations is smaller than
  `tolerance_value`.

  Can be `0` for no value threshold.

- `tolerance_parameter`:

  \[`numeric(1)`\]  
  A non-negative tolerance value. The AO process terminates if the
  distance between the current estimate and the before
  `tolerance_history` iterations is smaller than `tolerance_parameter`.

  Can be `0` for no parameter threshold.

  By default, the distance is measured using the euclidean norm, but
  another norm can be specified via the `tolerance_parameter_norm`
  field.

- `tolerance_parameter_norm`:

  \[`function`\]  
  The norm that measures the distance between the current estimate and
  the one from the last iteration. If the distance is smaller than
  `tolerance_parameter`, the AO process is terminated.

  It must be of the form `function(x, y)` for two vector inputs `x` and
  `y`, and return a single `numeric` value. By default, the euclidean
  norm `function(x, y) sqrt(sum((x - y)^2))` is used.

- `tolerance_history`:

  \[`integer(1)`\]  
  The number of iterations to look back to determine whether
  `tolerance_value` or `tolerance_parameter` has been reached.

- `add_details`:

  \[`logical(1)`\]  
  Add details about the AO process to the output?

------------------------------------------------------------------------

### Method `print_status()`

Prints a status message.

#### Usage

    Process$print_status(message, message_type = 8, verbose = self$verbose)

#### Arguments

- `message`:

  \[`character(1)`\]  
  A status message.

- `message_type`:

  \[`integer(1)`\]  
  The message type, one of the following:

  - `1` for
    [`cli::cli_h1()`](https://cli.r-lib.org/reference/cli_h1.html)

  - `2` for
    [`cli::cli_h2()`](https://cli.r-lib.org/reference/cli_h1.html)

  - `3` for
    [`cli::cli_h3()`](https://cli.r-lib.org/reference/cli_h1.html)

  - `4` for
    [`cli::cli_alert_success()`](https://cli.r-lib.org/reference/cli_alert.html)

  - `5` for
    [`cli::cli_alert_info()`](https://cli.r-lib.org/reference/cli_alert.html)

  - `6` for
    [`cli::cli_alert_warning()`](https://cli.r-lib.org/reference/cli_alert.html)

  - `7` for
    [`cli::cli_alert_danger()`](https://cli.r-lib.org/reference/cli_alert.html)

  - `8` for
    [`cli::cat_line()`](https://cli.r-lib.org/reference/cat_line.html)

- `verbose`:

  \[`logical(1)`\]  
  Print tracing details during the AO process?

------------------------------------------------------------------------

### Method `initialize_details()`

Initializes the `details` part of the output.

#### Usage

    Process$initialize_details(initial_parameter, initial_value)

#### Arguments

- `initial_parameter`:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  The starting parameter values for the AO process.

- `initial_value`:

  \[`numeric(1)`\]  
  The function value at the initial parameters.

------------------------------------------------------------------------

### Method `update_details()`

Updates the `details` part of the output.

#### Usage

    Process$update_details(
      value,
      parameter_block,
      seconds,
      error,
      error_message,
      block = self$block
    )

#### Arguments

- `value`:

  \[`numeric(1)`\]  
  The updated function value.

- `parameter_block`:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  The updated parameter values for the active parameter block.

- `seconds`:

  \[`numeric(1)`\]  
  The time in seconds for solving the sub-problem.

- `error`:

  \[`logical(1)`\]  
  Did solving the sub-problem result in an error?

- `error_message`:

  \[`character(1)`\]  
  An error message if `error = TRUE`.

- `block`:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  The currently active parameter block, represented as parameter
  indices.

------------------------------------------------------------------------

### Method `get_partition()`

Get a parameter partition.

#### Usage

    Process$get_partition()

------------------------------------------------------------------------

### Method `get_details()`

Get the `details` part of the output.

#### Usage

    Process$get_details(
      which_iteration = NULL,
      which_block = NULL,
      which_column = c("iteration", "value", "parameter", "block", "seconds")
    )

#### Arguments

- `which_iteration`:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  Selects the iteration(s).

  Can also be `NULL` to select all iterations.

- `which_block`:

  \[`character(1)` \|
  [`integer()`](https://rdrr.io/r/base/integer.html)\]  
  Selects the parameter block in the partition and can be one of

  - `"first"` for the first parameter block,

  - `"last"` for the last parameter block,

  - an `integer` vector of parameter indices,

  - or `NULL` for all parameter blocks.

- `which_column`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Selects the columns in the `details` part of the output and can be one
  or more of

  - `"iteration"`,

  - `"value"`,

  - `"parameter"`,

  - `"block"`,

  - and `"seconds"`.

------------------------------------------------------------------------

### Method `get_value()`

Get the function value in different steps of the AO process.

#### Usage

    Process$get_value(
      which_iteration = NULL,
      which_block = NULL,
      keep_iteration_column = FALSE,
      keep_block_columns = FALSE
    )

#### Arguments

- `which_iteration`:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  Selects the iteration(s).

  Can also be `NULL` to select all iterations.

- `which_block`:

  \[`character(1)` \|
  [`integer()`](https://rdrr.io/r/base/integer.html)\]  
  Selects the parameter block in the partition and can be one of

  - `"first"` for the first parameter block,

  - `"last"` for the last parameter block,

  - an `integer` vector of parameter indices,

  - or `NULL` for all parameter blocks.

- `keep_iteration_column`:

  \[`logical(1)`\]  
  Keep the column containing the information about the iteration in the
  output?

- `keep_block_columns`:

  \[`logical(1)`\]  
  Keep the column containing the information about the active parameter
  block in the output?

------------------------------------------------------------------------

### Method `get_value_latest()`

Get the function value in the latest step of the AO process.

#### Usage

    Process$get_value_latest()

------------------------------------------------------------------------

### Method `get_value_best()`

Get the optimum function value in the AO process.

#### Usage

    Process$get_value_best()

------------------------------------------------------------------------

### Method `get_parameter()`

Get the parameter values in different steps of the AO process.

#### Usage

    Process$get_parameter(
      which_iteration = self$iteration,
      which_block = NULL,
      keep_iteration_column = FALSE,
      keep_block_columns = FALSE
    )

#### Arguments

- `which_iteration`:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  Selects the iteration(s).

  Can also be `NULL` to select all iterations.

- `which_block`:

  \[`character(1)` \|
  [`integer()`](https://rdrr.io/r/base/integer.html)\]  
  Selects the parameter block in the partition and can be one of

  - `"first"` for the first parameter block,

  - `"last"` for the last parameter block,

  - an `integer` vector of parameter indices,

  - or `NULL` for all parameter blocks.

- `keep_iteration_column`:

  \[`logical(1)`\]  
  Keep the column containing the information about the iteration in the
  output?

- `keep_block_columns`:

  \[`logical(1)`\]  
  Keep the column containing the information about the active parameter
  block in the output?

------------------------------------------------------------------------

### Method `get_parameter_latest()`

Get the parameter value in the latest step of the AO process.

#### Usage

    Process$get_parameter_latest(parameter_type = "full")

#### Arguments

- `parameter_type`:

  \[`character(1)`\]  
  Selects the parameter type and can be one of

  - `"full"` (default) to get the full parameter vector,

  - `"block"` to get the parameter values for the current block, i.e.,
    the parameters with the indices `self$block`

  - `"fixed"` to get the parameter values which are currently fixed,
    i.e., all except for those with the indices `self$block`

------------------------------------------------------------------------

### Method `get_parameter_best()`

Get the optimum parameter value in the AO process.

#### Usage

    Process$get_parameter_best(parameter_type = "full")

#### Arguments

- `parameter_type`:

  \[`character(1)`\]  
  Selects the parameter type and can be one of

  - `"full"` (default) to get the full parameter vector,

  - `"block"` to get the parameter values for the current block, i.e.,
    the parameters with the indices `self$block`

  - `"fixed"` to get the parameter values which are currently fixed,
    i.e., all except for those with the indices `self$block`

------------------------------------------------------------------------

### Method `get_seconds()`

Get the optimization time in seconds in different steps of the AO
process.

#### Usage

    Process$get_seconds(
      which_iteration = NULL,
      which_block = NULL,
      keep_iteration_column = FALSE,
      keep_block_columns = FALSE
    )

#### Arguments

- `which_iteration`:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  Selects the iteration(s).

  Can also be `NULL` to select all iterations.

- `which_block`:

  \[`character(1)` \|
  [`integer()`](https://rdrr.io/r/base/integer.html)\]  
  Selects the parameter block in the partition and can be one of

  - `"first"` for the first parameter block,

  - `"last"` for the last parameter block,

  - an `integer` vector of parameter indices,

  - or `NULL` for all parameter blocks.

- `keep_iteration_column`:

  \[`logical(1)`\]  
  Keep the column containing the information about the iteration in the
  output?

- `keep_block_columns`:

  \[`logical(1)`\]  
  Keep the column containing the information about the active parameter
  block in the output?

------------------------------------------------------------------------

### Method `get_seconds_total()`

Get the total optimization time in seconds of the AO process.

#### Usage

    Process$get_seconds_total()

------------------------------------------------------------------------

### Method `check_stopping()`

Checks if the AO process can be terminated.

#### Usage

    Process$check_stopping()
