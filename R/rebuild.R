#' Rebuild our current search path according to a list of locked packages.
#'
#' As with the \href{https://github.com/romainfrancois/nothing}{nothing package},
#' this function will unload everything except the base R packages from the
#' search path prior to re-building.
#'
#' @param packages list of locked_packages.
rebuild <- function(packages) {
  reset_search_path()

  lapply(packages, attach)
}

reset_search_path <- function() {
  repeat {
    pkgs <- setdiff(loadedNamespaces(), "base")
  }
}

attach <- function(locked_package) {
  locked_package$name
}

# https://github.com/wch/r-source/tree/trunk/src/library
native_namespaces <-
  c("base", "compiler", "datasets", "grDevices", "graphics", "grid",
    "methods", "parallel", "profile", "splines", "stats4", "tcltk",
    "tools", "translations", "utils")

