.onAttach = function(lib, pkg) {
  # startup message
  msg = c(paste0(
    "Thanks for using ao version ", packageVersion("ao")),
    ", happy alternating optimization!\n",
    "See https://loelschlaeger.github.io/ao for help.\n",
    "Type 'citation(\"ao\")' for citing this R package.")
  packageStartupMessage(msg)
  invisible()
}
