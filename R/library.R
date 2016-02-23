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

  install_package(structure(
    locked_package,
    class = c(remote, class(locked_package))
  ))
}

install_package <- function(locked_package) {
  if (!locked_package$is_dependency_package) {
    cat("Installing", crayon::green(locked_package$name),
      as.character(locked_package$version), "from", class(locked_package)[1], "\n")
  } else {
    cat("Installing dependency", crayon::blue(locked_package$name),
      "from", locked_package$remote, "\n")
  }
  UseMethod("install_package")
}

install_package.local <- function(locked_package) {
  stopifnot(is.element("dir", names(locked_package)))
  install_locked_package(locked_package,
    devtools::install(locked_package$dir,
                      quiet = notTRUE(getOption("lockbox.verbose"))))
}

# Helpfully borrowed from https://github.com/christophergandrud/repmis/blob/master/R/InstallOldPackages.R
# Did not simply import the function because it introduces too many dependencies
#' @author Kirill Sevastyanenko
install_old_CRAN_package <- function(name, version, repo = "http://cran.r-project.org") {
  # List available packages on the repo. Maybe we can simply install.packages?
  available <- available.packages(contriburl =
    contrib.url(repos = "http://cran.us.r-project.org", type = "source"))
  available <- data.frame(unique(available[, c("Package", "Version")]))
  pkg <- available[available$Package == name, ]

  # Simply install.packages if version happens to be the latest available on CRAN.
  # You can specify the fastest CRAN mirror by setting the `lockbox.CRAN_mirror` option
  # or Rstudio mirror will be used by default.
  repos <- getOption('lockbox.CRAN_mirror') %||% c(CRAN = "http://cran.rstudio.com")
  remote_version <- package_version(as.character(pkg$Version))
  if (dim(pkg)[1] == 1 && remote_version == version) {
    return(utils::install.packages(
      name, repos = repos, INSTALL_opts = "--vanilla",
      quiet = notTRUE(getOption('lockbox.verbose'))))
  }

  # If we did not find the package on CRAN - try CRAN archive.
  from <- paste0(repo, "/src/contrib/Archive/", name, "/", name, "_", version, ".tar.gz")
  pkg.tarball <- tempfile(fileext = ".tar.gz")
  download.file(url = from, destfile = pkg.tarball, quiet = notTRUE(getOption('lockbox.verbose')))

  # We need to switch directories to ensure no infinite loop happens when
  # the .Rprofile calls lockbox::lockbox.
  old_dir <- getwd()
  on.exit(setwd(old_dir))
  tmpdir <- file.path(tempdir(), "foo")
  dir.create(tmpdir, FALSE, TRUE)
  setwd(tmpdir)

  utils::install.packages(pkg.tarball, repos = NULL, type = "source",
                          INSTALL_opts = "--vanilla",
                          quiet = notTRUE(getOption("lockbox.verbose")))
  unlink(pkg.tarball)
}

install_package.CRAN <- function(locked_package) {
  # TODO: (RK) Fetch correct version? Support install from source?
  locked_package$repo <- locked_package$repo %||% "http://cran.r-project.org"
  install_locked_package(locked_package,
    install_old_CRAN_package(locked_package$name, locked_package$version))
}

#' @importFrom devtools install_github
install_package.github <- function(locked_package) {
  stopifnot(is.element("repo", names(locked_package)))

  ref <- locked_package$ref %||% locked_package$version
  install_locked_package(locked_package, {
    if (locked_package$is_dependency_package && is.null(locked_package$ref)) {
      repo_arg <- locked_package$repo
    } else {
      repo_arg <- paste(locked_package$repo, ref, sep = "@")
    }
    arguments <- list(
      repo_arg,
      reload = FALSE,
      quiet  = notTRUE(getOption('lockbox.verbose'))
    )
    if (nzchar(token <- Sys.getenv("GITHUB_PAT"))) {
      arguments$auth_token <- token
    }
    if (!is.null(locked_package$subdir)) {
      arguments$subdir <- locked_package$subdir
    }

    do.call(devtools::install_github, arguments)
  })
}

install_locked_package <- function(locked_package, installing_expr) {
  temp_library <- staging_library()
  pkgdir <- file.path(temp_library, locked_package$name)

  # For some reason, if the package already exists, R CMD INSTALL does not
  # let us install it.
  unlink(pkgdir, TRUE, TRUE)

  ## Pretend our library path is the staging library during installation.
  testthatsomemore::package_stub("base", ".libPaths", function(...) temp_library, {
    force(quietly(installing_expr))
  })

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
