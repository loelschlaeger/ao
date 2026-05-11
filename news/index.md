# Changelog

## ao 1.2.3

- Added value `estimate_split` to the output, which is `estimate` split
  by `target` (only when applicable).

- Set the default value for the number of iterations within each block
  to 10.

- Removed renv.

- Improved documentation.

- Added tests for invalid inputs and random partition determinism.

## ao 1.2.2

CRAN release: 2025-12-15

- Small bug fixes in non-user level functions `get_details()` and
  `.generate_random_partition()`

## ao 1.2.1

CRAN release: 2025-06-26

- Added information to `stopping_reason` when solving sub-problem fails.

- Adapted to [optimizeR](https://loelschlaeger.de/optimizeR/) version
  1.2.1.

## ao 1.2.0

CRAN release: 2025-04-25

- Parameter bounds, analytical gradient and Hessian are now supported
  more generally.

- Argument `add_details = FALSE` can be used to reduce information in
  the output.

## ao 1.1.1

- In the case of multiple AO processes, the output now contains the
  additional elements `estimates` and `values` with results for all
  processes.

## ao 1.1.0

CRAN release: 2024-07-13

- AO can now be run in multiple (possibly parallel) processes by
  providing a `list` of values for the `initial`, `partition`, and/or
  `base_optimizer` parameters, where each combination creates a separate
  AO process.

## ao 1.0.1

- Removed `update_code` from output. Updates are now accepted regardless
  whether they improve the function value. In the end, the best solution
  is returned.

- Added `tolerance_history` argument to compare function value and
  parameters between more than one iteration.

- Added `hide_warnings` argument to hide warnings during the AO process.

## ao 1.0.0

CRAN release: 2024-06-20

- Implemented random partitions, parameter bounds, new stopping criteria
  (iteration limit, time limit, change in function value, change in
  parameters).

- The [`ao()`](https://loelschlaeger.de/ao/reference/ao.md) API did
  change a lot, please check the documentation.

## ao 0.3.3

CRAN release: 2024-02-29

- Now using [cli](https://cli.r-lib.org) to improve error messages.

## ao 0.3.2

CRAN release: 2024-02-05

- Fixed [\#4](https://github.com/loelschlaeger/ao/issues/4) (thanks
  to S. Chib).

## ao 0.3.1

CRAN release: 2024-01-31

- Added package start up-message.

- Added more details to README file.

- Increased required version of
  [optimizeR](https://loelschlaeger.de/optimizeR/) to version 1.0.3.

## ao 0.3.0

CRAN release: 2023-11-08

- Updated code to the new
  [optimizeR](https://loelschlaeger.de/optimizeR/) version 1.0.1.

## ao 0.2.7

CRAN release: 2023-09-29

- Renamed input `print.level` -\> `verbose`.

- In the return value, renamed element `"time"` -\> `"seconds"` (time is
  now always in seconds); the same in the `"sequence"` element.
  Additionally, added `"value"` column to the `"sequence"` element.

- Added input `f_partition` to optionally specify functions for the
  partitions.

- Added input `joint_end` to optionally optimize jointly in the end of
  the AO process.

- Input `iterlim` can be `Inf` now.

- Having empty elements in `partition` is no longer allowed.

- Removed plotting feature for parameter updates during estimation.

- New dependency on [checkmate](https://mllg.github.io/checkmate/) for
  input checks.

## ao 0.2.6

CRAN release: 2023-03-31

- Added package logo and created package website
  (<https://loelschlaeger.de/ao/>) with the
  [`{pkgdown}`](https://pkgdown.r-lib.org/) package. No code changes.

## ao 0.2.5

CRAN release: 2023-01-23

- Updated code to the new
  [optimizeR](https://loelschlaeger.de/optimizeR/) version 0.3.0, which
  renamed changed some input and output names.

- In particular, in
  [`ao()`](https://loelschlaeger.de/ao/reference/ao.md), renamed input
  `optimizer` -\> `base_optimizer`.

- Fixed bug when using numerical gradient and/or Hessian in target
  function.

## ao 0.2.4

CRAN release: 2022-12-04

- Updated code to the new
  [optimizeR](https://loelschlaeger.de/optimizeR/) version 0.2.0, which
  renamed some functions.

## ao 0.2.3

CRAN release: 2022-09-02

- Fixed bugs when having `...` arguments for
  [`ao()`](https://loelschlaeger.de/ao/reference/ao.md).

## ao 0.2.2

CRAN release: 2022-08-24

- Removed the `set_f()` interface. The optimization problem is now
  specified directly in
  [`ao()`](https://loelschlaeger.de/ao/reference/ao.md).

- The output format of
  [`ao()`](https://loelschlaeger.de/ao/reference/ao.md) has been
  changed, see the documentation of
  [`ao()`](https://loelschlaeger.de/ao/reference/ao.md).

- Arbitrary optimizer can now be specified in
  [`ao()`](https://loelschlaeger.de/ao/reference/ao.md) as an
  `optimizer` object via the framework from the
  [`{optimizeR}`](https://CRAN.R-project.org/package=optimizeR) package.

- Removed utils functions from [ao](https://loelschlaeger.de/ao/). They
  are imported from the [optimizeR](https://loelschlaeger.de/optimizeR/)
  package.

## ao 0.2.1

CRAN release: 2022-01-04

- The [Himmelblau’s
  function](https://en.wikipedia.org/wiki/Himmelblau%27s_function) is
  included as an example.

## ao 0.2.0

- [`ao()`](https://loelschlaeger.de/ao/reference/ao.md) now returns the
  sequence of estimates in the different iterations as a data frame.

- [`ao()`](https://loelschlaeger.de/ao/reference/ao.md) now is able to
  plot the sequence of estimates during estimation.

- Argument `groups` in
  [`ao()`](https://loelschlaeger.de/ao/reference/ao.md) now is called
  `partition` and `sequence` is replaced by `iterations`.

- New function `set_f()` for specification of the optimization problem.
  Parameter constraints can now be imposed on the target function.

- New helper functions `is_number()`, `try_silent()`, `timed()` and
  `euclidean()`.

- Included a vignette with details on alternating optimization.

- New package sticker.

## ao 0.1.4

CRAN release: 2021-10-06

- Updated package metadata.

- Updated package sticker.

## ao 0.1.3

CRAN release: 2021-08-12

- Exported method `print.ao()`.

## ao 0.1.2

CRAN release: 2021-08-10

- Skip alternation optimization step if a group is empty.

- Estimation times now is returned always in seconds.

- Return [`stats::nlm()`](https://rdrr.io/r/stats/nlm.html) outputs.

- Implemented method `print.ao()`.

## ao 0.1.1

CRAN release: 2021-06-30

- Fixed minor bugs.

## ao 0.1.0

CRAN release: 2021-06-21

- Initial release.
