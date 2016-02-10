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

install_dependency <- function(dependency_package) {
  remote <- locked_package$remote %||% "CRAN"
  install_package(structure(
    locked_package,
    class = c(remote, class(locked_package))
  ))
}

#' Install CRAN dependencies first to minimize change of unresolved dependencies
install_dependencies <- function(dependency_packages) {
   is.cran <- vapply(dependency_packages, function(pack) is.null(pack$repo) ||
     identical(pack$repo, "CRAN"), logical(1))
   lapply(lock[is.cran], install_dependency)
   lapply(lock[!is.cran], install_dependency)
}

install_package <- function(locked_package) {
  cat("Installing", crayon::green(locked_package$name),
      as.character(locked_package$version), "from", class(locked_package)[1], "\n")
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
  download.file(url = from, destfile = pkg.tarball)

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
  # TODO: (RK) What if we just want latest from master?
  install_locked_package(locked_package, {
    arguments <- list(
      paste(locked_package$repo, ref, sep = "@"),
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

#' For installations that include dependencies not in lockbox, take care to
#' allow for unspecified versions
all_package_version_mismatch <- function(package) {
  if (is.na(package$version)) {
    if (is.na(current_version(package$name))) {
      TRUE
    } else{
      FALSE
    }
  } else{
    !identical(current_version(package), package$version)
  }
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

current_version.dependency_package <- function(package) {
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

get_ordered_dependencies <- function(lock, mismatches) {
   is.cran <- vapply(lock, function(lp) is.null(lp$repo), logical(1))
   lock_cran <- lock[is.cran]
   lock_repo <- lock[!is.cran]
   cat(paste("Retrieving dependencies..."))
   get_dependencies_for_list(lock[!is.cran & mismatches], lock)
}

get_dependencies_for_list <- function(master_list, lock) {
  current_dependencies <- list()
  for (i in 1:length(master_list)) {
    cat(".")
    package <- master_list[[i]]
    current_dependencies <- combine_dependencies(
      current_dependencies
      , get_remote_dependencies(
        structure(package, class = c(package$remote %||% "CRAN", class(master_list[[i]])))))
  }
  current_list <- add_details(combine_dependencies(current_dependencies, master_list), lock)
  current_list <- lapply(current_list, as.dependency_package)
  if (identical(master_list, current_list)) return(master_list)
  get_dependencies_for_list(current_list, lock)
}

#' Check a dependency list for inclusion in the lockfile and add those additional
#' details (repo, remote, version, subdir) if it does appear there
add_details <- function(current_list, lock) {
  lock_names <- vapply(lock, function(l) l$name, character(1))
  list_names <- vapply(current_list, function(l) l$name, character(1))
  lapply(current_list
    , function(el) {
      if (el$name %in% lock_names) {
        locked_package <- lock[[which(lock_names == el$name)[1]]]
        if (is.na(el$version) || identical(compareVersion(as.character(el$version), as.character(locked_package$version)), -1L)) {
          el$version <- as.character(locked_package$version)
        }
        if (identical(compareVersion(as.character(el$version), as.character(locked_package$version)), 1L)) {
          stop(paste0("Dependency: \'", el$name, ", Version: ", el$version
            , " is required, but lockbox is locked at version: ", as.character(locked_package$version)))
        }
        if ("ref" %in% names(locked_package)) {
          el$ref <- locked_package$ref
        }
        if ("repo" %in% names(locked_package)) {
          el$repo <- locked_package$repo
        }
        if ("remote" %in% names(locked_package)) {
          el$remote <- locked_package$remote
        }
        if ("subdir" %in% names(locked_package)) {
          el$subdir <- locked_package$subdir
        }
      }
      if (!"remote" %in% names(el)) {
         el$remote <- "CRAN"
      }
      el})
}

#' Combine two lists of dependencies via version comparisons, but keep packages
#' found in list1 on the left side (updating versions with corresponding list2
#' values if necessary) in order to preserve dependency order.
combine_dependencies <- function(list1, list2) {
  if (length(list1) == 0) return(list2)
  if (length(list2) == 0) return(list1)
  names1 <- lapply(list1, function(obj) obj$name)
  names2 <- lapply(list2, function(obj) obj$name)
  version1 <- lapply(list1, function(obj) obj$version)
  version2 <- lapply(list2, function(obj) obj$version)
  names(version1) <- names1
  names(version2) <- names2

  keep1 <- vapply(
    names1
    , function(n) {
      if (n %in% names2) {
        v1 <- version1[[n]]
        v2 <- version2[[n]]
        if (is.na(v2)) return(TRUE)
        if (is.na(v1)) return(FALSE)
        !identical(compareVersion(as.character(v1), as.character(v2)), -1)
      } else{
        TRUE
      }}
    , logical(1))

  swap_versions1  <- names1 %in% names2 & !keep1
  swap_versions2 <- vapply(names1[swap_versions1], function(n) which(names2 == n), integer(1))
  version1[swap_versions1] <- version2[swap_versions2]

  keep2  <- !names2 %in% names1
  names_final <- c(names1, names2[keep2])
  version_final <- c(version1, version2[keep2])
  Map(function(n,v) list(name = n, version = v), names_final, version_final)
}

get_remote_dependencies <- function(package) {
  UseMethod("get_remote_dependencies")
}

#' If a package is on CRAN dependency installation should go without a hitch
get_remote_dependencies.CRAN <- function(package) {
  list()
}

#' For packages on github we will either use the current library DESCRIPTION
#' file or download the accurate remote DESCRIPTION file.
get_remote_dependencies.github <- function(package) {
  is_local_dependency <- is.dependency_package(package) &&
     compareVersion(as.character(current_version(package$name)) , as.character(package$version)) == 1
  is_local_locked <- is.locked_package(package) && !version_mismatch(package)
  if (is_local_locked || is_local_dependency) {
    dcf <- description_file_for(package$name)
  } else{
    remote <- package$remote
    filepath <- download_package(structure(
      package,
      class = c(remote, class(package))))
    remote <- get_remote(package)
    dirname <- paste0(remote$username,"-",remote$repo,"-",remote$auth_token)
    file_list <- unzip(filepath, list = TRUE)
    subdir <- ""
    if (!is.null(package$subdir)){
      subdir <- paste0("/",package$subdir)
    }
    description_name <- file_list$Name[grepl(paste0("^[^/]+", subdir,"/DESCRIPTION"),file_list$Name)]
    dcf <- read.dcf(file = unz(filepath, description_name))
  }

  dependency_levels <- c("Depends", "Imports")
  dependency_levels %in% colnames(dcf)

  if (!any(dependency_levels %in% colnames(dcf))) return(list())

  if (all(dependency_levels %in% colnames(dcf))) {
    dependencies_parsed <- rbind(tools::package.dependencies(dcf, depLevel = c("Imports"))[[package$name]]
    , tools::package.dependencies(dcf, depLevel = c("Depends"))[[package$name]])
  } else{
    dependencies_parsed <- tools::package.dependencies(dcf
      , depLevel = dependency_levels[dependency_levels %in% colnames(dcf)])[[package$name]]
  }
  dependencies_parsed <- dependencies_parsed[!grepl("^[rR]$", dependencies_parsed[,1]), , drop = FALSE]
  if (identical(nrow(dcf),0L)) return(list())
  lapply(seq_along(dependencies_parsed[,1])
    , function(i) {
      list(name = as.character(dependencies_parsed[i,1]), version = as.character(dependencies_parsed[i,3]))
    })
}

download_package <- function(package) {
  UseMethod("download_package")
}

download_package.github <- function(package) {
  remote <- get_remote(package)
  quiet <- !isTRUE(getOption('lockbox.verbose'))
  devtools:::remote_download.github_remote(remote, quiet = quiet)
}

get_remote <- function(package) {
  ref <- package$ref %||% package$version
  arguments <- list(
    paste(package$repo, ref, sep = "@"))
  if (nzchar(token <- Sys.getenv("GITHUB_PAT"))) {
    arguments$auth_token <- token
  }
  if (!is.null(package$subdir)) {
    arguments$subdir <- package$subdir
  }
  remote <- do.call(devtools:::github_remote, arguments)
}
