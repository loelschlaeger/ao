# Merge optimization results

This helper function merges the results of multiple AO processes.

## Usage

``` r
merge_results(
  results,
  minimize = TRUE,
  add_details = TRUE,
  processes = data.frame()
)
```

## Arguments

- results:

  \[`list`\]  
  A `list` of outputs from
  [`ao`](https://loelschlaeger.de/ao/reference/ao.md).

- minimize:

  \[`logical(1)`\]  
  Minimize during the AO process?

  If `FALSE`, maximization is performed.

- add_details:

  \[`logical(1)`\]  
  Add details about the AO process to the output?

- processes:

  \[`data.frame`\]  
  A `data.frame` describing how the different processes were specified.

## Value

A `list`, see section "Output value" on the
[`ao`](https://loelschlaeger.de/ao/reference/ao.md) page.
