`%||%` <- function(x, y) if (is.null(x)) y else x

libPath <- function() {
  lib <- .libPaths()[1L]
  if (identical(lib, .Library)) {
    # We *never* want to accidentally manipulate the system library!
    stop("Something went wrong, restart R or lockbox.")
  }
  lib
}

lockbox_imports <- function() {
  dcf <- system.file("DESCRIPTION", package = "lockbox", lib.loc = .Library)
  Filter(nzchar, strsplit(read.dcf(dcf)[,"Imports"], "[\n, ]+")[[1]])
}

#' @author Kevin Ushey
#' @source \url{https://github.com/rstudio/packrat/blob/92492ebc882bd048f092238af033d8a6fd03902f/R/utils.R#L469}
symlink <- function(from, to) {
  if (is.windows()) Sys.junction(from, to)
  else file.symlink(from, to)
}

#' @author Kevin Ushey
#' @source \url{https://github.com/rstudio/packrat/blob/ce9fb1de3ed490d3f85b0cae4534a3e998db659e/R/platform.R#L1}
is.windows <- function() {
  Sys.info()["sysname"] == "Windows"
}

#' @author Kevin Ushey
#' @source \url{https://github.com/rstudio/packrat/blob/ce9fb1de3ed490d3f85b0cae4534a3e998db659e/R/platform.R#L5}
is.mac <- function() {
  Sys.info()["sysname"] == "Darwin"
}

#' @author Kevin Ushey
#' @source \url{https://github.com/rstudio/packrat/blob/ce9fb1de3ed490d3f85b0cae4534a3e998db659e/R/platform.R#L9}
is.linux <- function() {
  Sys.info()["sysname"] == "Linux"
}


#' @author Kevin Ushey
#' @source \url{https://github.com/rstudio/packrat/blob/649097381ad702c56e6326ae4cee5c56713f6276/R/library-support.R#L140}
is.symlink <- function(path) {

  ## Strip trailing '/'
  path <- gsub("/*$", "", path)

  ## Sys.readlink returns NA for error, "" for 'not a symlink', and <path> for symlink
  ## return false for first two cases, true for second
  result <- Sys.readlink(path)
  if (is.na(result)) FALSE
  else nzchar(result)
}

quietly <- function(expr) {
  suppressPackageStartupMessages(suppressMessages(suppressWarnings(expr)))
}

package_version_from_path <- function(pkg_path) {
  package_version(unname(read.dcf(file.path(pkg_path, "DESCRIPTION"))[, "Version"]))
}

#' @useDynLib lockbox duplicate_lockbox_
duplicate <- function(x) {
  .Call(duplicate_lockbox_, x, PACKAGE = "lockbox")
}
