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
lockbox <- function(file_or_list, env = getOption("lockbox.env", "!packages")) {
  UseMethod("lockbox", file_or_list)
}

#' @export
lockbox.character <- function(file, env) {
  lockbox(yaml::yaml.load_file(file), env)
}

#' @export
lockbox.list <- function(lock, env) {
  library(devtools)
  if (missing(env)) env <- "!packages"
  if (is.null(lock$packages))
    stop("Invalid config. Make sure your config format is correct")
  lock <-
    if (identical(env, "!packages") || is.null(lock[[env]])) {
      lock$packages
    } else {
      lock <- lapply(lock$packages, function(package) {
        if(package$name %in% lock[[env]]) package else NULL
      })
      lock <- lock[!sapply(lock, is.null)]
    }

  lock <- lapply(lock, as.locked_package)
  disallow_special_packages(lock)
  disallow_duplicate_packages(lock)

  set_transient_library()

  ## Find the packages whose version does not match the current library.
  mismatches <- vapply(lock, version_mismatch, logical(1))

  sapply(lock[!mismatches], function(locked_package) {
    cat('Using', crayon::green(locked_package$name), as.character(locked_package$version), '\n')
  })

  ## Find the packages that will need to be installed
  already_in_lockbox <- vapply(lock, exists_in_lockbox, logical(1))

  if (any(mismatches & !already_in_lockbox)) {
    all_packages <- get_ordered_dependencies(lock, mismatches & !already_in_lockbox)
    all_packages <- lapply(all_packages, reset_to_latest_version)
    all_packages <- all_packages[vapply(all_packages
      , version_mismatch
      , logical(1))]
    all_packages <- c(lock[!(mismatches & !already_in_lockbox)], all_packages)
  } else{
    all_packages <- lock[mismatches]
  }

  quietly({
    ## Replace our library so that it has these packages instead.
    align(all_packages)

    ## And re-build our search path.
    rebuild(all_packages)
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
  locked_package
}

as.locked_package <- function(list) {
  stopifnot(is.element("name", names(list)),
            is.element("version", names(list)))

  if (is.null(list$is_dependency_package)) {
    list$is_dependency_package <- FALSE
  }

  if (is.element("repo", names(list)) && !is.element("remote", names(list))) {
    list$remote <- "github"
  }

  if (is.na(list$version) && !list$is_dependency_package) {
      stop(sprintf("NA version specification for locked package %s",
                 sQuote(list$name)))
  } else if(!is.na(list$version)) {
    if (is.na(package_version(list$version))) {
      stop(sprintf("Invalid package %s version %s.",
                 sQuote(list$name), sQuote(list$version)))
    }
    list$version <- package_version(list$version)
  }

  # TODO: (RK) Support CRAN version dependencies.
  structure(list, class = "locked_package")
}

is.locked_package <- function(obj) is(obj, "locked_package")

#' The secret lockbox library path.
lockbox_library <- function() {
  getOption("lockbox.directory") %||% normalizePath("~/.R/lockbox", mustWork = FALSE)
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
  package_names    <- vapply(lock, `[[`, character(1), "name")

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
  locked_names <- vapply(lock, function(p) p$name, character(1))
  if( any(duplicated(locked_names))) {
    stop(paste0("The following packages are duplicated in your lockfile: "
      , paste(locked_names[duplicated(locked_names)], collapse = ", ")))
  }
}
