# ao 0.2.6

* Just added package logo and created package website (https://loelschlaeger.de/ao/) with the [{pkgdown}](https://pkgdown.r-lib.org/) package. No code changes.

# ao 0.2.5

* Updated code to the new {optimizeR} version 0.3.0, which renamed changed some input and output names.

* In particular, in `ao()`, renamed input `optimizer` -> `base_optimizer`.

* Fixed bug when using numerical gradient and/or Hessian in target function.

# ao 0.2.4

* Updated code to the new {optimizeR} version 0.2.0, which renamed some functions.

# ao 0.2.3

* Fixed bugs when having `...` arguments for `ao()`.

# ao 0.2.2

* Removed the `set_f()` interface. The optimization problem is now specified directly in `ao()`.

* The output format of `ao()` has been changed, see the documentation of `ao()`.

* Arbitrary optimizer can now be specified in `ao()` as an `optimizer` object via the framework from [the {optimizeR} package](https://CRAN.R-project.org/package=optimizeR).

* Removed utils functions from {ao}. They are imported from the {optimizeR} package.

# ao 0.2.1

* The [Himmelblau's function](https://en.wikipedia.org/wiki/Himmelblau%27s_function) is included as an example.

# ao 0.2.0

* `ao()` now returns the sequence of estimates in the different iterations as a data frame.

* `ao()` now is able to plot the sequence of estimates during estimation.

* Argument `groups` in `ao()` now is called `partition` and `sequence` is replaced by `iterations`.

* New function `set_f()` for specification of the optimization problem. Parameter constraints can now be imposed on the target function.

* New utils functions `is_number()`, `try_silent()`, `timed()` and `euclidean()`.

* Included a vignette for details on alternating optimization.

* New package sticker.

# ao 0.1.4

* Updated package metadata.

* Updated package sticker.

# ao 0.1.3

* Exported method `print.ao()`.

# ao 0.1.2

* Skip alternation optimization step if a group is empty.

* Estimation times now is returned always in seconds.

* Return `stats::nlm()` outputs.

* Implemented method `print.ao()`.

# ao 0.1.1

* Fixed minor bugs.

# ao 0.1.0

* Initial version.
