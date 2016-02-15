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

install_dependency_package <- function(dependency_package) {
  remote <- dependency_package$remote %||% "CRAN"
  cat("Installing dependency", crayon::blue(dependency_package$name),
    "from", remote, "\n")
  if (identical(remote, "CRAN")) {
      swap_libpaths()
      utils::install.packages(dependency_package$name
        , INSTALL_opts = "--vanilla", type = "source"
        , quiet = notTRUE(getOption("lockbox.verbose")))
      swap_libpaths()
  } else {
    install_package(structure(
      dependency_package
      , class = c(dependency_package$remote, class(dependency_package))
      , quiet = notTRUE(getOption("lockbox.verbose"))
    ))
  }
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
    if (is.dependency_package(locked_package)) {
      swap_libpaths()
    }

    repeat_count <- 0
    while (repeat_count < 5) {
      if (repeat_count == 4) {
        warning("Could not download package: ", locked_package$name, ", version: "
                , locked_package$version, "from github.", call. = FALSE)
        break
      }
      result <- tryCatch(do.call(devtools::install_github, arguments), error = function(e)e)
      if (!is(result, "error")) break
      repeat_count <- repeat_count + 1
    }

    if (is.dependency_package(locked_package)) {
      swap_libpaths()
    }
  })
}

swap_libpaths <- function() {
  .libPaths(c(.libPaths()[3L]
    , .libPaths()[2L]
    , .libPaths()[1L]
    , .libPaths()[seq_along(.libPaths()) > 3]))
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
#' @param package locked_package or dependency package
#' @return TRUE or FALSE according as the current library's package version
#'   is incorrect.
version_mismatch <- function(package) {
  if (is.dependency_package(package)) {
    if (is.na(current_version(package))) {
      TRUE
    } else{
      if (is.na(package$version)) {
        FALSE
      } else {
        current_version(package) < package$version
      }
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
current_version <- function(pkg, libP = libPath()) {
  UseMethod("current_version")
}

current_version.character <- function(package_name, libP = libPath()) {
  dcf <- description_file_for(package_name, libP)
  if (is.null(dcf)) {
    NA
  } else {
    package_version(unname(dcf[,"Version"]))
  }
}

current_version.locked_package <- function(package, libP = libPath()) {
  current_version(package$name)
}

current_version.dependency_package <- function(package, libP = .libPaths()[3L]) {
  current_version(package$name, libP)
}

description_file_for <- function(package_name, libP) {
  dcf_file <- file.path(libP, package_name, "DESCRIPTION")
  if (file.exists(dcf_file)) {
    read.dcf(dcf_file)
  } else {
    NULL
  }
}

#' Get dependencies for all elements with lock, but only do so for current
#' version mismatches and non-cran installations
get_ordered_dependencies <- function(lock, mismatches) {
   cat(crayon::blue(paste("Retrieving dependency info...")))
   deps <- get_dependencies_for_list(lock[mismatches], lock, list(), "")
   cat("\n")
   deps
}

#' Recursive function to take a list and lock and extract dependencies, sorting
#' along the way using the combine_dependencies function.
get_dependencies_for_list <- function(master_list, lock, previously_parsed_deps, current_parent) {
  current_dependencies <- master_list
  for (i in 1:length(master_list)) {
    package <- master_list[[i]]
    if (!is_previously_parsed(package, previously_parsed_deps)) {
      single_package_dependencies <- get_dependencies(
       structure(package
         , class = c(package$remote %||% "CRAN"
           , class(package))))
      previously_parsed_deps[[length(previously_parsed_deps) + 1]] <- list(
        package = package
        , dependencies = single_package_dependencies)
    } else{
      single_package_dependencies <- previously_parsed_deps[[which_previously_parsed(
        package, previously_parsed_deps)]][["dependencies"]]
    }
    current_dependencies <- combine_dependencies(
      single_package_dependencies
      , current_dependencies
      , package$name)
  }
  current_list <- add_details(current_dependencies, lock)
  current_list <- lapply(current_list, as.dependency_package)
  current_list <- reset_to_locked(current_list, lock)
  if (identical(master_list, current_list)) return(master_list)
  get_dependencies_for_list(current_list, lock, previously_parsed_deps, current_parent)
}

#' Have we previously gotten this packages dependencies?
is_previously_parsed <- function(package, previously_parsed_deps) {
  location <- which_previously_parsed(package, previously_parsed_deps)
  if (identical(location, 0L)) {
    FALSE
  } else{
    TRUE
  }
}

#' Where, if anywhere, have I stored this packages/version combinations'
#' previously parsed dependencies?
which_previously_parsed <- function(package, previously_parsed_deps) {
  if (length(previously_parsed_deps) == 0) return(0L)
  previously_parsed_names <- vapply(previously_parsed_deps
    , function(p) p[["package"]]$name, character(1))
  if (package$name %in% previously_parsed_names) {
    sel <- which(previously_parsed_names == package$name)
    subsel <- vapply(previously_parsed_deps[sel]
      , function(p) identical(p[["package"]]$version, package$version)
      , logical(1))
    if (any(subsel)) {
      return(sel[subsel])
    }
  }
  0L
}

#' Check a dependency list for inclusion in the lockfile and add those additional
#' details (repo, remote, version, subdir) if it does appear there.  Also throw
#' an error if we require a dependency version greater than that specified by
#' the lockfile.
add_details <- function(current_list, lock) {
  lock_names <- vapply(lock, function(l) l$name, character(1))
  lapply(current_list
    , function(el) {
      if (el$name %in% lock_names) {
        locked_package <- lock[[which(lock_names == el$name)[1]]]
        if (is.na(el$version) || package_version(as.character(el$version)) <
          package_version(as.character(locked_package$version))) {
            el$version <- as.character(locked_package$version)
        }
        if (package_version(as.character(el$version)) >
          package_version(as.character(locked_package$version))) {
            stop(paste0("Dependency: \'", el$name, ", Version: ", el$version
              , " is required, but lockbox is locked at version: "
              , as.character(locked_package$version)))
        }
        fields_to_replace <- c("ref", "repo", "remote", "subdir", "dir", "load")
        invisible(lapply(fields_to_replace, function(field) {
          if (field %in% names(locked_package)) {
            el[[field]] <<- locked_package[[field]]
          }}))
      }
      if (!"remote" %in% names(el) || is.na(el$remote)) {
        el$remote <- "CRAN"
      }
      el})
}

#' Combine two lists of dependencies via version comparisons.  Keep packages
#' found in list1 on the left side of the entirety of list2, while moving
#' the parent package to the space after it's rightmost dependency found in list2.
#' Update versions with greatest values if necessary.
combine_dependencies <- function(list1, list2, current_parent) {
  names1 <- vapply(list1, function(obj) obj$name, character(1))
  names2 <- vapply(list2, function(obj) obj$name, character(1))

  version1 <- vapply(list1, function(obj) as.character(obj$version), character(1))
  version2 <- vapply(list2, function(obj) as.character(obj$version), character(1))

  # Certain packages are no longer on cran but incorporated into R Core
  core_pkgs <- as.character(installed.packages(priority = "base")[,1])
  version1 <- version1[!names1 %in% core_pkgs]
  list1 <- list1[!names1 %in% core_pkgs]
  names1 <- names1[!names1 %in% core_pkgs]
  version2 <- version2[!names2 %in% core_pkgs]
  list2 <- list2[!names2 %in% core_pkgs]
  names2 <- names2[!names2 %in% core_pkgs]

  if (length(list1) == 0) return(list2)
  if (length(list2) == 0) return(list1)

  names(version1) <- names1
  names(version2) <- names2
  keep1 <- !names1 %in% names2

  if (current_parent %in% names2 && any(names2 %in% names1)) {
    new_selection <- !names1 %in% names2
    init_parent_slot <- which(names2 == current_parent)
    final_parent_slot <- max(which(names2 %in% names1))
    if (final_parent_slot > init_parent_slot) {
      sel1 <- seq_along(names2) != init_parent_slot & seq_along(names2) <= final_parent_slot
      sel2 <- seq_along(names2) > final_parent_slot
      version2 <- c(version2[sel1], version2[init_parent_slot], version2[sel2])
      names2 <- c(names2[sel1], names2[init_parent_slot], names2[sel2])
    }
  }

  version2 <- swap_versions(names1, names2, version1, version2)

  names_final <- c(names1[keep1], names2)
  version_final <- c(version1[keep1], version2)
  Map(function(n,v) list(name = n, version = v), names_final, version_final)
}

#' Swap versions information when side1 is greater than side2
swap_versions <- function(names1, names2, version1, version2) {
  swap_version2for1 <- vapply(
    names1
    , function(n) {
      if (n %in% names2) {
        if (is.na(version2[[n]])) return(TRUE)
        if (is.na(version1[[n]])) return(FALSE)
        package_version(version1[[n]]) >= package_version(version2[[n]])
      } else{
        FALSE
      }}
    , logical(1))
  swap_versions1  <- names1 %in% names2 & swap_version2for1
  swap_versions2 <- vapply(names1[swap_versions1]
    , function(n) which(names2 == n)
    , integer(1))
  version2[swap_versions2] <- version1[swap_versions1]
  version2
}

#' Either use the current library DESCRIPTION
#' file or download the accurate remote DESCRIPTION file.
get_dependencies <- function(package) {
  is_local_dependency <- is.dependency_package(package) &&
    !is.na(current_version(package)) &&
    (is.na(package$version) ||
      package_version(as.character(current_version(package))) <=
      package_version(as.character(package$version)))
  if (is_local_dependency) {
    dependencies_from_description(package, description_file_for(package$name, .libPaths()[3L]))
  } else {
    cat(crayon::blue("."))
    get_remote_dependencies(package)
  }
}

get_remote_dependencies <- function(package) {
  UseMethod("get_remote_dependencies")
}

#' If a package is local we just read from the directory given
get_remote_dependencies.local <- function(package) {
  description_name <- file_list$Name[grepl(paste0("^[^/]+"
    ,"/DESCRIPTION$"), file_list$Name)]
  dcf <- read.dcf(description_name)
  dependencies_from_description(package, dcf)
}

#' For packages on CRAN we will extract to a temporary directory when we
#' download the accurate remote DESCRIPTION file.
get_remote_dependencies.CRAN <- function(package) {
  remote <- package$remote
  filepath <- download_package(structure(
    package,
    class = c(remote, class(package))))
  split_fp <- strsplit(filepath, "/")[[1]]
  dirpath <- paste(split_fp[-length(split_fp)], collapse = "/")
  file_list <- untar(filepath, list = TRUE)
  description_name <- file_list[grepl(paste0("^[^/]+"
    ,"/DESCRIPTION$"), file_list)]
  untar(filepath, description_name, exdir = dirpath)
  description_path <- paste0(dirpath, "/", description_name)
  dcf <- read.dcf(file = description_path)
  unlink(filepath)
  dependencies_from_description(package, dcf)
}

#' Download the accurate remote DESCRIPTION file for a github repo.
get_remote_dependencies.github <- function(package) {
  remote <- package$remote
  filepath <- download_package(structure(
    package,
    class = c(remote, class(package))))
  file_list <- unzip(filepath, list = TRUE)
  subdir <- ""
  if (!is.null(package$subdir)){
    subdir <- paste0("/",package$subdir)
  }
  description_name <- file_list$Name[grepl(paste0("^[^/]+"
    , subdir
    ,"/DESCRIPTION$"), file_list$Name)]
  file_con <- unz(filepath, description_name)
  dcf <- read.dcf(file = file_con)
  close(file_con)
  unlink(filepath)
  dependencies_from_description(package, dcf)
}

#' Parse dependencies from description using the tools package
dependencies_from_description <- function(package, dcf) {
  dependency_levels <- c("Depends", "Imports")
  dependency_levels %in% colnames(dcf)
  if (!any(dependency_levels %in% colnames(dcf))) return(list())
  if (all(dependency_levels %in% colnames(dcf))) {
    dependencies_parsed <- rbind(
      tools::package.dependencies(dcf, depLevel = c("Imports"))[[package$name]]
      , tools::package.dependencies(dcf, depLevel = c("Depends"))[[package$name]])
  } else{
    dependencies_parsed <- tools::package.dependencies(dcf
      , depLevel = dependency_levels[dependency_levels %in%
        colnames(dcf)])[[package$name]]
  }
  dependencies_parsed <- dependencies_parsed[!grepl("^[rR]$"
    , dependencies_parsed[,1]), , drop = FALSE]
  if (identical(nrow(dcf),0L)) return(list())
  lapply(seq_along(dependencies_parsed[,1])
    , function(i) {
      list(name = as.character(dependencies_parsed[i,1])
        , version = as.character(dependencies_parsed[i,3]))
    })
}

download_package <- function(package) {
  UseMethod("download_package")
}

#' Download CRAN package, either current or older version
download_package.CRAN <- function(package) {
  remote_version <- get_available_cran_version(package)
  name <- package$name
  version <- package$version
  repo = "http://cran.r-project.org"
  if (!is.locked_package(package)) {
    version <- NA
  }

  ###Some packages are available in archive only
  if (is.na(remote_version)) {
    archive_addition <- paste0("Archive/", name, "/")
    url <- paste0(repo, "/src/contrib/", archive_addition)
    filenames <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
    filenames <- paste(url, strsplit(filenames, "\r*\n")[[1]], sep = "")
    filenames <- filenames[grepl(paste0(name, "_[0-9\\.\\-]+\\.tar\\.gz"),filenames)]
    expr <- gregexpr(paste0(name, "_([0-9\\.\\-]+)\\.tar\\.gz"), filenames)
    filenames <- vapply(regmatches(filenames, expr), function(match) match[1], character(1))
    archived_version <- gsub("(.*_)([0-9\\.\\-]+)(\\.tar\\.gz)", "\\2", filenames[length(filenames)])
    if (is.na(version) || package_version(archived_version) > package_version(as.character(version))) {
      version <- archived_version
    }
  } else{
    # Simply download latest if version happens to be the latest available on CRAN.
    remote_version <- as.character(remote_version)
    if (is.na(version) || package_version(remote_version) == package_version(version)) {
      version <- remote_version
      archive_addition <- ""
    } else{
      archive_addition <- paste0("Archive/", name, "/")
    }
  }

  from <- paste0(repo, "/src/contrib/", archive_addition, name, "_", version, ".tar.gz")
  pkg_tarball <- tempfile(fileext = ".tar.gz")
  out <- suppressWarnings(tryCatch(
    download.file(url = from, destfile = pkg_tarball, quiet = notTRUE(getOption('lockbox.verbose')))
    , error = function(e) e))
  # Sometimes the current version isn't accessible in it's usual place, but is already archived
  if (is(out, "error")) {
    archive_addition <- paste0("Archive/", name, "/")
    from <- paste0(repo, "/src/contrib/", archive_addition, name, "_", version, ".tar.gz")
    download.file(url = from, destfile = pkg_tarball, quiet = notTRUE(getOption('lockbox.verbose')))
  }
  pkg_tarball
}

get_available_cran_version <- function(package, repo = "http://cran.r-project.org") {
  repo = "http://cran.r-project.org"

  # List available packages on the repo
  available <- available.packages(contriburl =
    contrib.url(repos = "http://cran.us.r-project.org", type = "source"))
  available <- data.frame(unique(available[, c("Package", "Version")]))
  pkg <- available[available$Package == package$name, ]

  if (nrow(pkg) == 0) {
    NA
  } else{
    pkg$Version
  }
}

#' Download a package from github using devtools' remote_download function
download_package.github <- function(package) {
  remote <- get_remote(package)
  quiet <- !isTRUE(getOption('lockbox.verbose'))
  devtools:::remote_download.github_remote(remote, quiet = quiet)
}

#' Create remote in form devtools' remote_download likes.
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
