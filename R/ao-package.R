#' @aliases ao-package
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom checkmate test_flag
#' @importFrom checkmate test_function
#' @importFrom checkmate test_list
#' @importFrom checkmate test_number
#' @importFrom cli cli_abort
#' @importFrom cli style_hyperlink
#' @importFrom oeli test_numeric_vector
#' @importFrom optimizeR Optimizer
#' @importFrom R6 R6Class
#' @importFrom utils packageVersion
## usethis namespace: end
NULL

#' @noRd

.onAttach <- function(lib, pkg) {
  doc_link <- "https://loelschlaeger.de/ao"
  msg <- c(
    paste0(
      "Thanks for using {ao} version ", utils::packageVersion("ao")
    ),
    ", happy alternating optimization!\n",
    "Documentation: ",
    cli::style_hyperlink(doc_link, doc_link)
  )
  packageStartupMessage(msg)
  invisible()
}
