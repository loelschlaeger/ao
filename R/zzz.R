.onAttach = function(lib, pkg) {
  # startup message
  msg = c(paste0(
    "ao version ", packageVersion("ao")),
    "\nType 'citation(\"ao\")' for citing this R package in publications.",
    "\nSee https://github.com/loelschlaeger/ao for references.")
  packageStartupMessage(msg)
  invisible()
}
