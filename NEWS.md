# ao 0.1.4.9000

* New function `set_f()` for specification of the optimization problem. Parameter constrains can now be imposed on the target function.

* `ao()` now returns the sequence of estimates in the different iterations as a data frame.

* Plot function in `ao()`.

* Argument `groups` in `ao()` now is called `partition`, `sequence` is replaced by `iterations`.

* New utils functions `is_number()`, `try_silent()` and `timed()`.

* New package sticker.

# ao 0.1.4

* Updated package metadata.

* Updated package sticker.

# ao 0.1.3

* Exported method `print.ao()`.

# ao 0.1.2

* Skip alternation optimization step if a group is empty.

* Estimation times now is returned always in seconds.

* Return nlm outputs.

* Implemented method `print.ao()`.

# ao 0.1.1

* Fixed minor bugs.

# ao 0.1.0

* Initial version.
