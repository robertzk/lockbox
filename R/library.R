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

is.local_package <- function(locked_package) {
  identical(locked_package$remote, "local")
}

is.autoinstall_package <- function(locked_package) {
  is.local_package(locked_package) && isTRUE(locked_package$autoinstall)
}

lockbox_package_path <- function(locked_package, library = lockbox_library()) {
  ## The final package is stored in lockbox_lib_path/pkg_name/version/pkg_name
  ## The trailing pkg_name is to ensure help files work, since these depend
  ## on the directory name:
  ## https://github.com/wch/r-source/blob/ed66b715221d2720f5b334470335635bada520b1/src/library/utils/R/help.R#L213
  file.path(library, locked_package$name
    , package_version(as.character(locked_package$version))
    , locked_package$name)
}

lockbox_package_download_path <- function(locked_package, library = lockbox_library()) {
  file.path(lockbox_download_dir(), locked_package$name
    , paste0(
      as.character(locked_package$remote %||% "CRAN")
      , gsub("/.*", "", locked_package$repo)
      , as.character(locked_package$ref %||% locked_package$version)
      , get_extension(locked_package)))
}

`place_in_lockbox!` <- function(locked_package) {
  install_locked_package(locked_package)
}

get_extension <- function(package) {
  if (is.null(package$repo) || package$remote == "CRAN") return(".tar.gz")
  ".zip"
}

load_package <- function(locked_package) {
  stopifnot(is.element("dir", names(locked_package)))
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Autoloading packages requires devtools. Please `install.packages('devtools')`.")
  }
  cat("Reinstalling", crayon_green(locked_package$name),
    as.character(locked_package$version), "from", locked_package$dir, "\n")
  devtools::load_all(locked_package$dir)
}

install_package <- function(locked_package, libPath, quiet) {
  UseMethod("install_package")
}

install_package.local <- function(locked_package, libPath, quiet) {
  stopifnot(is.element("dir", names(locked_package)))

  utils::install.packages(locked_package$dir, lib = libPath, repos = NULL
    , type = "source", quiet = quiet)
}

install_package.CRAN <- function(locked_package, libPath, quiet) {
  ## We already downloaded a source file when parsing DESCRIPTIONS, so try that
  ## first
  try(utils::install.packages(locked_package$download_path, lib = libPath
    , repos = NULL, type = "source", quiet = quiet))
  pkgdir <- file.path(libPath, locked_package$name)

  ## Last chance at installation if compilation fails.  Install through normal
  ## CRAN channels
  if (!file.exists(pkgdir)) {
    locked_package$latest_version <- locked_package$latest_version %||% get_latest_version(locked_package)
    if (package_version(locked_package$version) == package_version(locked_package$latest_version)) {
      repos <- getOption('lockbox.CRAN_mirror') %||% c(CRAN = "http://cran.r-project.org")
      install.packages(locked_package$name, repos = repos
        , type = get_download_type(locked_package), lib = libPath, quiet = quiet)
    }
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
  package_root <- extracted_dir <- file.path(parent_dir, extracted_dir)
  unzip(filepath, exdir = parent_dir)
  if (!is.null(locked_package$subdir)) {
    extracted_dir <- file.path(extracted_dir, locked_package$subdir)
  }
  install_from_dir(extracted_dir, libPath, quiet, package_root)
}

install_from_dir <- function(extracted_dir, libPath, quiet, package_root = extracted_dir) {
  ## Make every file in the extracted directory executable to cover compilation
  ## edge-cases
  unlink(list.files(libPath, full.names = TRUE, pattern = "^00LOCK"),
         recursive = TRUE, force = TRUE)
  lapply(list.files(package_root, full.names = TRUE, recursive = TRUE), Sys.chmod, "0777")

  utils::install.packages(extracted_dir, lib = libPath, repos = NULL
    , type = "source", quiet = quiet)
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
  if (methods::is(output, "error") || !file.exists(pkgdir)) {
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

download_package <- function(package, force = FALSE) {
  download_path <- lockbox_package_download_path(package)
  if (isTRUE(force)) unlink(download_path, force = TRUE)
  if (file.exists(download_path)) return(download_path)
  download_dir <- dirname(download_path)
  if (!file.exists(download_dir)) {
    out <- dir.create(download_dir, FALSE, TRUE)
  }
  UseMethod("download_package")
}

## Download CRAN package, either current or older version
download_package.CRAN <- function(package, force) {
  pkg_tarball <- lockbox_package_download_path(package)
  remote_version <- get_available_cran_version(package)
  name <- package$name
  version <- package$version
  repo <- getOption('lockbox.CRAN_mirror') %||% c(CRAN = "http://cran.r-project.org")
  ref <- NULL

  ## Simply download latest if version happens to be the latest available on CRAN.
  remote_version <- as.character(remote_version)
  if (is.na(package$version) || package_version(remote_version) == package_version(version)) {
    version <- remote_version
    archive_addition <- ""
  } else {
    archive_addition <- paste0("Archive/", name, "/")
    ref <- package$ref
  }

  from <- paste0(repo, "/src/contrib/", archive_addition, name, "_"
    , ref %||% version, ".tar.gz")

  out <- suppressWarnings(tryCatch(
    download.file(url = from, destfile = pkg_tarball
    , quiet = notTRUE(getOption('lockbox.verbose'))), error = function(e) e))

  ## Sometimes the current version isn't accessible in it's usual place,
  ## but is already archived
  if (methods::is(out, "error")) {
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
download_package.github <- function(package, force) {
  dest <- lockbox_package_download_path(package)
  if (is.na(package$version)) {
    package$version <- NULL
  }
  remote <- get_remote(package)
  remote_download.github_remote(remote, dest, quiet = !isTRUE(getOption('lockbox.verbose')))
}

get_download_type <- function(package) {
  if (package$is_dependency_package && .Platform$pkgType != "source") "binary"
  else "source"
}

## Return the latest available version of a package from CRAN
get_available_cran_version <- function(package, repo = "http://cran.r-project.org") {
  type <- get_download_type(package)
  available <- available.packages(contriburl = contrib.url(repos = repo, type = type)
    , filter = c("OS_type", "subarch", "duplicates"))
  available <- data.frame(unique(available[, c("Package", "Version")]))

  ## If package is only available in source redo this call
  if (!package$name %in% available$Package && type != "source") {
    available <- available.packages(contriburl = contrib.url(repos = repo, type = "source")
    , filter = c("OS_type", "subarch", "duplicates"))
    available <- data.frame(unique(available[, c("Package", "Version")]))
  }
  if (!package$name %in% available$Package) {
    prefix <- "Locked"
    parent_string <- ""
    if (package$is_dependency_package) {
      prefix <- "Dependency"
      parent_string <- paste0(" from parent package ", package$parent_package)
    }
    stop(paste0(prefix, " Package ", package$name, parent_string, " is not available on CRAN."
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

  token <- github_pat(quiet = notTRUE(getOption('lockbox.verbose')))
  if (!is.null(token)) {
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
