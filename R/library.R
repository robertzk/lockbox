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
#' @name ensure_package_exists_in_lockbox
`ensure_package_exists_in_lockbox!` <- function(locked_package) {
  if (!exists_in_lockbox(locked_package)) {
    `place_in_lockbox!`(locked_package)
  }
}

exists_in_lockbox <- function(locked_package) {
  file.exists(lockbox_package_path(locked_package))
}

lockbox_package_path <- function(locked_package, library = lockbox_library()) {
  # The final package is stored in lockbox_lib_path/pkg_name/version/pkg_name
  # The trailing pkg_name is to ensure help files work, since these depend
  # on the directory name:
  # https://github.com/wch/r-source/blob/ed66b715221d2720f5b334470335635bada520b1/src/library/utils/R/help.R#L213
  file.path(library, locked_package$name, locked_package$version, locked_package$name)
}

`place_in_lockbox!` <- function(locked_package) {
  remote <- locked_package$remote %||% "CRAN"

  install_locked_package(structure(
    locked_package,
    class = c(remote, class(locked_package))
  ))
}

install_package <- function(locked_package, libP) {
  if (!locked_package$is_dependency_package) {
    cat("Installing", crayon::green(locked_package$name),
      as.character(locked_package$version), "from", class(locked_package)[1], "\n")
  } else {
    cat("Installing dependency", crayon::blue(locked_package$name),
      "from", locked_package$remote, "\n")
  }
  fn <- UseMethod("install_package")
  output <- tryCatch(fn(), error = function(e) e)

  ## If we have an error during installation try again and show everything
  if (is(output, "error")) {
    option(lockbox.verbose = TRUE)
    fn()
  }
}

install_package.local <- function(locked_package, libP) {
  stopifnot(is.element("dir", names(locked_package)))

  utils::install.packages(locked_package$dir, lib = libP, repos = NULL, type = "source",
    INSTALL_opts = "--vanilla",
    quiet = notTRUE(getOption("lockbox.verbose")))
}

install_package.CRAN <- function(locked_package, libP) {
  filepath <- locked_package$download_path
  utils::install.packages(filepath, lib = libP, repos = NULL, type = "source",
    INSTALL_opts = "--vanilla",
    quiet = notTRUE(getOption("lockbox.verbose")))
  unlink(locked_package$filepath)
}

install_package.github <- function(locked_package, libP) {
  stopifnot(is.element("repo", names(locked_package)))
  ref <- locked_package$ref %||% locked_package$version

  filepath <- locked_package$download_path
  subdir <- ""
  if (!is.null(locked_package$subdir)) {
    subdir <- paste0("/",package$subdir)
  }
  extracted_filepath <- unzip(filepath
    , exdir = gsub("/[^/]+$","",filepath))[1]
  extracted_dir <- gsub("/[^/]+$","", extracted_filepath)
  utils::install.packages(extracted_dir, lib = libP, repos = NULL, type = "source",
    INSTALL_opts = "--vanilla",
    quiet = notTRUE(getOption("lockbox.verbose")))
  unlink(extracted_dir)
  unlink(locked_package$filepath)
}

install_locked_package <- function(locked_package, installing_expr) {
  temp_library <- staging_library()
  pkgdir <- file.path(temp_library, locked_package$name)

  # For some reason, if the package already exists, R CMD INSTALL does not
  # let us install it.
  unlink(pkgdir, TRUE, TRUE)

  ## Pretend our library path is the staging library during installation.
  install_package(locked_package, temp_library)

  if (!file.exists(pkgdir)) {
    unlink(temp_library, TRUE, TRUE)
    stop("Must have installed the package ",
         crayon::red(as.character(locked_package$name)),
         " of version ", sQuote(as.character(locked_package$version)))
  }

  if ((ver <- package_version_from_path(pkgdir)) != locked_package$version) {
    unlink(temp_library, TRUE, TRUE)
    stop(sprintf(paste0(
      "Incorrect version of package %s installed. Expected ",
      "%s but downloaded %s instead."), sQuote(locked_package$name),
      sQuote(locked_package$version), sQuote(ver)), call. = FALSE)
  }

  copy_real_packages_to_lockbox_library(temp_library)
  unlink(temp_library, TRUE, TRUE)
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
current_version <- function(pkg) {
  UseMethod("current_version")
}

current_version.character <- function(package_name) {
  dcf <- description_file_for(package_name, libPath())
  if (is.null(dcf)) {
    NA
  } else {
    package_version(unname(dcf[,"Version"]))
  }
}

current_version.locked_package <- function(package) {
  current_version(package$name)
}

description_file_for <- function(package_name, libP) {
  dcf_file <- file.path(libP, package_name, "DESCRIPTION")
  if (file.exists(dcf_file)) {
    read.dcf(dcf_file)
  } else {
    NULL
  }
}
