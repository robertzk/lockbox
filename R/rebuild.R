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
  repeat {
    pkgs <- setdiff(.packages(), c(native_namespaces, "lockbox"))
    if (!length(pkgs)) break
    for (pkg in pkgs) {
      try(detach(paste0("package:", pkg),
                 character.only = TRUE, force = TRUE),
          silent = TRUE)
    }
  }
}

attach <- function(locked_package) {
  library(locked_package$name, character.only = TRUE, lib.loc = libPath())
  locked_package$name
}

# https://github.com/wch/r-source/tree/trunk/src/library
native_namespaces <-
  c("base", "compiler", "datasets", "grDevices", "graphics", "grid",
    "methods", "parallel", "profile", "splines", "stats4", "tcltk",
    "tools", "translations", "utils")

