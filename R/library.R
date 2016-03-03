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
  ## The final package is stored in lockbox_lib_path/pkg_name/version/pkg_name
  ## The trailing pkg_name is to ensure help files work, since these depend
  ## on the directory name:
  ## https://github.com/wch/r-source/blob/ed66b715221d2720f5b334470335635bada520b1/src/library/utils/R/help.R#L213
  file.path(library, locked_package$name, locked_package$version, locked_package$name)
}

`place_in_lockbox!` <- function(locked_package) {
  install_locked_package(locked_package)
}

install_package <- function(locked_package, libPath, quiet) {
  UseMethod("install_package")
}

install_package.local <- function(locked_package, libPath, quiet) {
  stopifnot(is.element("dir", names(locked_package)))

  utils::install.packages(locked_package$dir, lib = libPath, repos = NULL
    , type = "source", INSTALL_opts = "--vanilla", quiet = quiet)
}

install_package.CRAN <- function(locked_package, libPath, quiet) {
  try(utils::install.packages(locked_package$download_path, lib = libPath
    , repos = NULL, type = "source", INSTALL_opts = "--vanilla", quiet = quiet))
  pkgdir <- file.path(libPath, locked_package$name)

  ## Last chance at installation if compilation fails
  if (!file.exists(pkgdir) &&
    package_version(locked_package$version) == package_version(locked_package$latest_version)) {
    repos <- getOption('lockbox.CRAN_mirror') %||% c(CRAN = "http://cran.r-project.org")
    install.packages(locked_package$name, INSTALL_opts = "--vanilla", repos = repos
      , lib = libPath, quiet = quiet)
  }
}

install_package.github <- function(locked_package, libPath, quiet) {
  stopifnot(is.element("repo", names(locked_package)))

  filepath <- locked_package$download_path
  parent_dir <- dirname(filepath)

  ## Be very careful here to get the extracted package directory
  ## while maintaining cross-platform support
  extracted_dir <- gsub(paste0(.Platform$file.sep, ".*"), ""
    , unzip(filepath, list = TRUE)$Name[1])
  extracted_dir <- file.path(parent_dir, extracted_dir)
  unzip(filepath, exdir = parent_dir)
  if (!is.null(locked_package$subdir)) {
    extracted_dir <- file.path(extracted_dir, locked_package$subdir)
  }
  install_from_dir(extracted_dir, libPath, quiet)
}

install_from_dir <- function(extracted_dir, libPath, quiet) {
  ## It's absolutely necessary that we make these executable
  Sys.chmod(file.path(extracted_dir,"configure"), "0777")
  Sys.chmod(file.path(extracted_dir,"configure.win"), "0777")
  Sys.chmod(file.path(extracted_dir,"cleanup"), "0777")

  utils::install.packages(extracted_dir, lib = libPath, repos = NULL
    , type = "source", INSTALL_opts = "--vanilla", quiet = quiet)
}

install_locked_package <- function(locked_package) {
  if (!locked_package$is_dependency_package) {
    cat("Installing", crayon_green(locked_package$name),
      as.character(locked_package$version), "from", class(locked_package)[1], "\n")
  } else {
    cat("Installing dependency", crayon_blue(locked_package$name),
      "from", locked_package$remote, "\n")
  }
  temp_library <- staging_library()
  pkgdir <- file.path(temp_library, locked_package$name)

  ## For some reason, if the package already exists, R CMD INSTALL does not
  ## let us install it.
  unlink(pkgdir, TRUE, TRUE)

  ## Install to the staging library
  output <- tryCatch(install_package(locked_package, temp_library
    , notTRUE(getOption("lockbox.verbose")))
    , error = function(e) e)

  ## If we have an error during installation try again and show everything
  if (is(output, "error") || !file.exists(pkgdir)) {
    try(install_package(locked_package, temp_library, FALSE))
    unlink(temp_library, TRUE, TRUE)
    stop("Must have installed the package ",
         crayon_red(as.character(locked_package$name)),
         " of version ", sQuote(as.character(locked_package$version)))
  }

  ver <- package_version_from_path(pkgdir)
  if (ver != locked_package$version) {
    unlink(temp_library, TRUE, TRUE)
    stop(sprintf(paste0(
      "Incorrect version of package %s installed. Expected ",
      "%s but downloaded %s instead."), sQuote(locked_package$name),
      sQuote(locked_package$version), sQuote(ver)), call. = FALSE)
  }


  copy_real_packages_to_lockbox_library(temp_library)
  unlink(temp_library, TRUE, TRUE)
}

is_installed_to_staging <- function(locked_package, pkgdir) {
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

description_file_for <- function(package_name, libPath) {
  dcf_file <- file.path(libPath, package_name, "DESCRIPTION")
  if (file.exists(dcf_file)) {
    read.dcf(dcf_file)
  } else {
    NULL
  }
}

download_package <- function(package) {
  UseMethod("download_package")
}

## Download CRAN package, either current or older version
download_package.CRAN <- function(package) {
  remote_version <- get_available_cran_version(package)
  name <- package$name
  version <- package$version
  repo <- getOption('lockbox.CRAN_mirror') %||% c(CRAN = "http://cran.r-project.org")
  ref <- NULL

  ## Dependency package means that we grab the latest.  download.packages
  ## does a better job grabbing the latest than we do.  Edge-cases where certain
  ## things have been updated on the information page for a package but others
  ## have not.
  if (package$is_dependency_package) {
    pkg_tarball <- tempfile(fileext = ".tar.gz", tmpdir = lockbox_download_dir())
    sep <- .Platform$file.sep
    dest_dir <- gsub(paste0(sep, "[^", sep, "]+$"), "", pkg_tarball)
    return(download.packages(package$name, dest_dir, repos = repo
      , type = "source", quiet = notTRUE(getOption('lockbox.verbose')))[1, 2])
  }

  ## Simply download latest if version happens to be the latest available on CRAN.
  remote_version <- as.character(remote_version)
  if (package_version(remote_version) == package_version(version)) {
    version <- remote_version
    archive_addition <- ""
  } else {
    archive_addition <- paste0("Archive/", name, "/")
    ref <- package$ref
  }

  from <- paste0(repo, "/src/contrib/", archive_addition, name, "_"
    , ref %||% version, ".tar.gz")

  pkg_tarball <- tempfile(fileext = ".tar.gz", tmpdir = lockbox_download_dir())
  out <- suppressWarnings(tryCatch(
    download.file(url = from, destfile = pkg_tarball
    , quiet = notTRUE(getOption('lockbox.verbose'))), error = function(e) e))

  ## Sometimes the current version isn't accessible in it's usual place,
  ## but is already archived
  if (is(out, "error")) {
    archive_addition <- paste0("Archive/", name, "/")
    from <- paste0(repo, "/src/contrib/", archive_addition, name, "_"
      , ref %||% version , ".tar.gz")
    download.file(url = from, destfile = pkg_tarball
      , quiet = notTRUE(getOption('lockbox.verbose')))
  }

  pkg_tarball
}

## Download a package from github using our version of devtools' remote_download
## function
download_package.github <- function(package) {
  if (is.na(package$version)) {
    package$version <- NULL
  }
  remote <- get_remote(package)
  remote_download.github_remote(remote, quiet = !isTRUE(getOption('lockbox.verbose')))
}

## Return the latest available version of a package from CRAN
get_available_cran_version <- function(package, repo = "http://cran.r-project.org") {
  available <- available.packages(contriburl =
    contrib.url(repos = repo, type = "source"))
  available <- data.frame(unique(available[, c("Package", "Version")]))
  if (!package$name %in% available$Package) {
    prefix <- "Locked"
    if (package$is_dependency_package) prefix <- "Dependency"
    warning(paste0(prefix, " Package ", package$name, " is not available on CRAN."
      , " Do you need to specify this package's repo?"))
  }
  pkg <- available[available$Package == package$name, ]
  if (nrow(pkg) == 0) {
    NULL
  } else {
    pkg$Version
  }
}

## Create remote in form devtools' remote_download  function likes.
get_remote <- function(package) {
  ref <- package$ref %||% package$version
  if (is.null(ref)) arguments <- list(package$repo)
  else arguments <- list(paste(package$repo, ref, sep = "@"))

  token <- Sys.getenv("GITHUB_PAT")
  if (nzchar(token)) {
    arguments$auth_token <- token
  } else {
      warning(crayon_red("Warning: "), "To download private repositories, please set up your ",
        sQuote(crayon_yellow("GITHUB_PAT")), " by following the instructions at ",
        "https://help.github.com/articles/creating-an-access-token-for-command-line-use/")
  }
  if (!is.null(package$subdir)) {
    arguments$subdir <- package$subdir
  }
  do.call(github_remote, arguments)
}
