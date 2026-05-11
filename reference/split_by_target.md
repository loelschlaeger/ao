# Split estimate by target

This helper function splits the solution by target parameters (if
provided), which is used for the output.

## Usage

``` r
split_by_target(estimate, target = NULL, npar)
```

## Arguments

- estimate:

  \[`numeric`\]  
  A parameter vector.

- target:

  \[[`character()`](https://rdrr.io/r/base/character.html) \| `NULL`\]  
  The name(s) of the argument(s) over which `f` is optimized.

  This can only be `numeric` arguments.

  If `NULL` (default), the first argument of `f` is optimized.

- npar:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  The length(s) of the target argument(s).

  Must be specified if more than one target argument is specified via
  the `target` argument.

  Can be `NULL` if there is only one target argument, in which case
  `npar` is set to `length(initial)`.

## Value

A (named) `list`, a partition of `estimate` according to `npar`.

## Author

Siddhartha Chib
