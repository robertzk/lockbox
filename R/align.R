#' Symlink a package with the appropriate version into the current library.
#'
#' @param locked_package locked_package. In particular, the \code{version}
#'    and \code{name} elements will be used.
#' @param verbose logical. Whether or not to announce usage of the package.
#'    By default \code{TRUE}.
align <- function(locked_package, verbose = TRUE) {
  if (is.list(locked_package) && !is.locked_package(locked_package)) {
    lapply(locked_package, align)
    ## Unlink the download directory to remove temporary files
    unlink(lockbox_download_dir(), TRUE, TRUE)
    return()
  }

  stopifnot(is.locked_package(locked_package))

  ## Make sure we have this package version in the lockbox secret library.
  `ensure_package_exists_in_lockbox!`(locked_package)

  ## Symlink the locked package to the correct lockbox version.
  `symlink_to_lockbox!`(locked_package)

  if (isTRUE(verbose)) {
    announce_package_usage(locked_package$name, locked_package$version)
  }
}

`symlink_to_lockbox!` <- function(locked_package) {
  path <- file.path(libPath(), locked_package$name)
  if (is.symlink(path)) unlink(path, force = TRUE)
  symlink(lockbox_package_path(locked_package), path)
}
