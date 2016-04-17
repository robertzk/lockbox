#' Re-organize Search Path to Use Lockbox Library.
#'
#' The lockbox package provides a separate directory, by default under
#' \code{"~/.R/.lockbox"} (although this is configurable from the global ption
#' \code{"lockbox.directory"}) that maintains different versions of packages
#' on demand. When a given set of versioned packages is requested, lockbox will
#' unload \emph{all other packages} and ensure only the given set of packages
#' with their respective versions are present.
#'
#' Since lockbox maintains a separate directory for its library, it will not
#' interfere with R's usual packages or libraries when R is restarted.
#'
#' @export
#' @param file_or_list character or list. A yaml-based lock file or its
#'    parsed out list format. This set of packages will be loaded into the
#'    search path and \emph{all other packages will be unloaded}.
#' @param env character. The name of the entry in the lockfile that contains
#'    package information.
lockbox <- function(file_or_list, env = getOption("lockbox.env", "!packages")) {
  UseMethod("lockbox")
}

#' @export
lockbox.character <- function(file, env) {
  lockbox(yaml::yaml.load_file(file), env)
}

#' @export
lockbox.list <- function(lock, env) {
  if (missing(env)) env <- "!packages"
  if (is.null(lock$packages)) stop("Invalid config. Make sure your config format is correct")
  if (identical(env, "!packages") || is.null(lock[[env]])) {
    lock <- lock$packages
  } else {
    lock <- lock$packages[vapply(lock$packages, `[[`, character(1), "name") %in% lock[[env]]]
  }

  lock <- lapply(lock, as.locked_package)
  disallow_special_packages(lock)
  disallow_duplicate_packages(lock)

  set_transient_library()
  set_download_dir()

  ## Add dependencies to lock
  lock <- get_ordered_dependencies(lock)
  lock <- lapply(lock, reset_to_latest_version)
  cat("\n")

  ## Find the packages whose version does not match the current library.
  mismatches <- vapply(lock, version_mismatch, logical(1))
  autoinstall_packages <- vapply(lock, function(lock) is.autoinstall_package(lock), logical(1))
  load_these_packages <- mismatches | autoinstall_packages

  sapply(lock[!load_these_packages], function(locked_package) {
    if (locked_package$is_dependency_package) {
      cat('Using dependency', crayon_blue(locked_package$name), as.character(locked_package$version), '\n')
    } else {
      cat('Using', crayon_green(locked_package$name), as.character(locked_package$version), '\n')
    }
  })

  quietly({
    ## Replace our library so that it has these packages instead.
    align(lock[load_these_packages])

    ## And re-build our search path. Do so in the reverse order of dependencies.
    rebuild(rev(lock))
  })
}

#' @export
lockbox.default <- function(obj) {
  stop(
    "Invalid parameters passed to ", sQuote("lockbox"), " method: ",
    "must be a ", sQuote("character"), " or ", sQuote("list"), " but ",
    "instead I got a ", sQuote(class(obj)[1]), "."
  )
}

reset_to_latest_version <- function(locked_package) {
  if (locked_package$is_dependency_package) {
    locked_package$version <- locked_package$latest_version
  }
  locked_package$version <- package_version(locked_package$version)
  locked_package
}

set_download_dir <- function() {
  download_dir <- lockbox_download_dir()
  if (!file.exists(download_dir)) dir.create(download_dir, FALSE, TRUE)
}

as.locked_package <- function(list) {
  stopifnot(is.element("name", names(list)),
            is.element("version", names(list)))

  list$is_dependency_package <- isTRUE(list$is_dependency_package %||% FALSE)

  if (is.element("repo", names(list)) && !is.element("remote", names(list))) {
    list$remote <- "github"
  }

  if (!list$is_dependency_package && is.na(package_version(list$version))) {
    stop(sprintf("Invalid package %s version %s.",
                 sQuote(list$name), sQuote(list$version)))
  } else if (!list$is_dependency_package) {
    ## This solves the inconsistent x.y-a.b naming convention problems that
    ## arise when transforming to a package_version.
    list$ref <- list$ref %||% as.character(list$version)
  }

  structure(list, class = "locked_package")
}

is.locked_package <- function(obj) is(obj, "locked_package")

#' The secret lockbox library path.
lockbox_library <- function() {
  getOption("lockbox.directory") %||% normalizePath("~/.R/lockbox", mustWork = FALSE)
}

#' The lockbox download path.
lockbox_download_dir <- function() {
  getOption("lockbox.download_dir") %||% file.path(lockbox_library(), "lockbox_download_dir")
}

#' The transient lockbox library path.
lockbox_transient_dir <- function() {
  getOption("lockbox.transient_dir") %||%
    normalizePath("~/.R/lockbox_transient", mustWork = FALSE)
}

#' The transient staging lockbox library path.
#' 
#' This will be used to copy interactively installed packages to
#' the vanilla library.
lockbox_transient_staging_dir <- function() {
  paste0(lockbox_transient_dir(), "_staging")
}

disallow_special_packages <- function(lock) {
  package_names <- vapply(lock, `[[`, character(1), "name")

  if ("lockbox" %in% package_names) {
    stop("Lockbox cannot manage itself, Mr. Hofstadter.", call. = FALSE)
  }

  if (any(package_names %in% special_namespaces)) {
    stop("Version maintenance of the following packages is not currently ",
      "supported by lockbox: ",
      paste(intersect(special_namespaces, package_names), collapse = ", "),
      ".", call. = FALSE)
  }
}

disallow_duplicate_packages <- function(lock) {
  locked_names <- vapply(lock, `[[`, character(1), "name")
  if (any(duplicated(locked_names))) {
    stop(paste0("The following packages are duplicated in your lockfile: "
      , paste(unique(locked_names[duplicated(locked_names)]), collapse = ", ")))
  }
}
