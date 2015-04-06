#' Ensure package name and version exists in the lockbox secret library.
#'
#' The lockbox package keeps a secret directory with the packages for
#' each given version. By default, this is in
#' \code{getOption("lockbox.lib")} or \code{"~/.R/lockbox"} if that option
#' is not set.
#'
#' @param locked_package locked_package. In particular, \code{name} and 
#'    \code{version} elements will be used. If the package version is
#'    not present in the lockbox, we will attempt to download it from
#'    CRAN or github.
#' @note The weird name is a derivative of a \href{http://stackoverflow.com/questions/612189/why-are-exclamation-marks-used-in-ruby-methods}{Rubyism}
#'    to indicate some serious side effects can occur! In this case, we 
#'    download the package from CRAN or github if the name + version combo
#'    does not exist.
`ensure_package_exists_in_lockbox!` <- function(locked_package) {
  if (!exists_in_lockbox(locked_package)) {
    `place_in_lockbox!`(locked_package)
  }
}

exists_in_lockbox <- function(locked_package) {
  file.exists(lockbox_package_path(locked_package))
}

lockbox_package_path <- function(locked_package) {
  file.path(lockbox_dir(), locked_package$name, locked_package$version)
}

`place_in_lockbox!` <- function(locked_package) {
  remote <- locked_package$remote %||% "CRAN"
  install_package(structure(
    locked_package,
    class = c(remote, class(locked_package))
  ))
}

install_package <- function(locked_package) {
  UseMethod("install_package")
}

install_package.CRAN <- function(locked_package) {
  # TODO: (RK) Fetch correct version?
  install_locked_package(locked_package, install.packages(locked_package$name))
}

#' @importFrom devtools install_github
install_package.github <- function(locked_package) {
  stopifnot(is.element("repo", names(locked_package)))

  ref <- locked_package$ref %||% locked_package$version
  install_locked_package(locked_package, {
    install_github(
      paste(locked_package$repo, ref, sep = "@"),
      reload = FALSE
    )
  })
}

install_locked_package <- function(locked_package, installing_expr) {
  tempdir <- file.path(lockbox_dir(), locked_package$name, "download")
  dir.create(tempdir, FALSE, TRUE)

  on.exit({
    pkgdir <- file.path(tempdir, locked_package$name)
    stopifnot(file.exists(pkgdir)) # Must have installed the package.
    newdir <- file.path(dirname(tempdir), as.character(locked_package$version))
    file.rename(pkgdir, newdir)
    unlink(tempdir, TRUE, TRUE)
  })

  ## Pretend our library path is the temporary library for this particular
  ## package name.
  testthatsomemore::package_stub("base", ".libPaths", function() tempdir, {
    force(installing_expr)
  })
}

#' Find packages whose version does not match the current library's version.
#' 
#' @param locked_package locked_package.
#' @return TRUE or FALSE according as the current library's package version
#'   is incorrect.
version_mismatch <- function(locked_package) {
  !identical(current_version(locked_package), locked_package$version)
}

#' The current version of this package in the current library.
#'
#' @param pkg character or locked_package. The name of the package.
#' @return a \code{\link{package_version}} object representing the version of
#'   this package in the current library.
current_version <- function(package_name) {
  UseMethod("current_version")
}

current_version.character <- function(package_name) {
  dcf <- description_file_for(package_name)
  if (is.null(dcf)) {
    NA
  } else {
    package_version(unname(dcf[,"Version"]))
  }
}

current_version.locked_package <- function(package) {
  current_version(package$name)
}

description_file_for <- function(package_name) {
  dcf_file <- file.path(libPath(), package_name, "DESCRIPTION")
  if (file.exists(dcf_file)) {
    read.dcf(dcf_file)
  } else {
    NULL
  }
}

