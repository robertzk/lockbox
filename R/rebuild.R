#' Rebuild our current search path according to a list of locked packages.
#'
#' As with the \href{https://github.com/romainfrancois/nothing}{nothing package},
#' this function will unload everything except the base R packages from the
#' search path prior to re-building.
#'
#' @param packages list of locked_packages.
rebuild <- function(packages) {
  reset_search_path()

  invisible(vapply(packages, attach, character(1)))
}

reset_search_path <- function() {
  # Helpfully borrowed from https://github.com/romainfrancois/nothing/blob/master/R/zzz.R
  native_loaded_namespace <- intersect(loadedNamespaces(), native_namespaces)
  on.exit(lapply(native_loaded_namespace, loadNamespace), add = TRUE)
  repeat_count <- 0

  repeat {
    pkgs <- setdiff(loadedNamespaces(), special_namespaces)
    if (!length(pkgs)) break
    if (repeat_count > 25) {
      warning("Could not unload the following namespaces when loading ",
              "lockfile: ", paste(sQuote(pkgs), collapse = ", "), call. = FALSE)
      break
    }

    for (pkg in pkgs) {
      try(unloadNamespace(pkg), silent = TRUE)
    }
    repeat_count <- repeat_count + 1
  }
}

attach <- function(locked_package) {
  if (!identical(locked_package$load, FALSE)) {
    library(locked_package$name, character.only = TRUE)
  }
  locked_package$name
}

# https://github.com/wch/r-source/tree/trunk/src/library
native_namespaces <-
  c("base", "stats", "compiler", "datasets", "grDevices", "graphics", "grid",
    "methods", "parallel", "profile", "splines", "stats4", "tcltk",
    "tools", "translations", "utils", "lattice", "Matrix")

# # These namespaces are particularly difficult to unload because of dependencies.
# pesky_namespaces <-
#   c("lockbox", "httr", "RCurl", "bitops", "crayon", "yaml", "testthat",
#     "testthatsomemore", "stringr", "digest", "lubridate", "memoise",
#     "plyr", "magrittr", "devtools", "Rcpp", "roxygen")

# These namespaces are particularly difficult to unload because of dependencies.
pesky_namespaces <- c("lockbox")

special_namespaces <- c(native_namespaces, pesky_namespaces)

