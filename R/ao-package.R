#' @aliases ao-package
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom optimizeR Optimizer
#' @importFrom utils packageVersion
## usethis namespace: end
NULL

#' @noRd

.onAttach <- function(lib, pkg) {
  msg <- paste0(
    "Thanks for using {ao} ", utils::packageVersion("ao"),
    ", happy alternating optimization!\n",
    "Documentation: https://loelschlaeger.de/ao"
  )
  packageStartupMessage(msg)
  invisible()
}
