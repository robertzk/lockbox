#' Symlink a package with the appropriate version into the current library.
#'
#' @param locked_package locked_package. In particular, the \code{version}
#'    and \code{name} elements will be used.
align <- function(locked_package) {
  stopifnot(is.locked_package(locked_package)) 

  ## Make sure we have this package version in the lockbox secret library.
  `ensure_package_exists_in_lockbox!`(locked_package)

  ## Symlink the locked package to the correct lockbox version.
  `symlink_to_lockbox!`(locked_package)
}

