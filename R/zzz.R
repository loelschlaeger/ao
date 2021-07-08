.onAttach = function(lib, pkg) {
  # startup message
  msg = c(paste0(
    "Thanks for installing ao version ",packageVersion("ao")),".",
    "\nType 'citation(\"ao\")' for citing this R package in your publications.",
    "\nThe project is stored at https://github.com/loelschlaeger/ao.")
  packageStartupMessage(msg)
  invisible()
}
