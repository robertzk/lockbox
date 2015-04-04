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
#' @note The weird name is a derivative of a \url[=Rubyism]{http://stackoverflow.com/questions/612189/why-are-exclamation-marks-used-in-ruby-methods}
#'    to indicate some serious side effects can occur! In this case, we 
#'    download the package from CRAN or github if the name + version combo
#'    does not exist.
`ensure_package_exists_in_lockbox!` <- function(locked_package) {
  
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

