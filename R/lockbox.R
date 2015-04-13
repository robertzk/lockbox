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
lockbox <- function(file_or_list) {
  UseMethod("lockbox")
}

#' @export
lockbox.character <- function(file) {
  lockbox(yaml::yaml.load_file(file))
}

#' @export
lockbox.list <- function(lock) {
  set_transient_library()

  lock <- lapply(lock, as.locked_package)
  disallow_special_packages(lock)
  
  ## Find the packages whose version does not match the current library.
  mismatches <- vapply(lock, version_mismatch, logical(1))

  quietly({
    ## Replace our library so that it has these packages instead.
    align(lock[mismatches])

    ## And re-build our search path.
    rebuild(lock) 
  })
}

as.locked_package <- function(list) {
  stopifnot(is.element("name", names(list)),
            is.element("version", names(list)))

  if (is.element("repo", names(list)) && !is.element("remote", names(list))) {
    list$remote <- "github"
  }

  if (is.na(package_version(list$version))) {
    stop(sprintf("Invalid package %s version %s.", 
                 sQuote(list$name), sQuote(list$version)))
  } else {
    list$version <- package_version(list$version)
  }

  # TODO: (RK) Support CRAN version dependencies.
  structure(list, class = "locked_package")
}

is.locked_package <- function(obj) is(obj, "locked_package")

#' @export
lockbox.default <- function(obj) {
  stop(
    "Invalid parameters passed to ", sQuote("lockbox"), " method: ",
    "must be a ", sQuote("character"), " or ", sQuote("list"), " but ",
    "instead I got a ", sQuote(class(obj)[1]), "."
  )
}

#' The secret lockbox library path.
lockbox_library <- function() {
  getOption("lockbox.directory") %||% normalizePath("~/.R/lockbox", mustWork = FALSE)
}

#' The transient lockbox library path.
lockbox_transient_dir <- function() {
  getOption("lockbox.transient_dir") %||%
    normalizePath("~/.R/lockbox_transient", mustWork = FALSE)
}

disallow_special_packages <- function(lock) {
  package_names    <- vapply(lock, `[[`, character(1), "name")
  if (any(package_names %in% special_namespaces)) {
    stop("Version maintenance of the following packages is not currently ",
      "supported by lockbox: ",
      paste(intersect(special_packages, package_names), collapse = ", "),
      ".", call. = FALSE)
  }
}
