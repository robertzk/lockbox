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
#' @param file_or_list character or list. A yaml-based lock file or its
#'    parsed out list format. This set of packages will be loaded into the
#'    search path and \emph{all other packages will be unloaded}.
lockbox <- function(file_or_list) {
  UseMethod("lockbox")
}

lockbox.character <- function(file) {
  lockbox(yaml::yaml.load_file(file))
}

lockbox.list <- function(lock) {
  lock <- lapply(lock, as.locked_package)
  
  ## Find the packages whose version does not match the current library.
  mismatch <- vapply(lock, version_mismatch, logical(1))
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

lockbox.default <- function(obj) {
  stop(
    "Invalid parameters passed to ", sQuote("lockbox"), " method: ",
    "must be a ", sQuote("character"), " or ", sQuote("list"), " but ",
    "instead I got a ", sQuote(class(obj)[1]), "."
  )
}


# lockfile <- yaml::yaml.load_file(file.path(root(), 'config', 'lockfile.yml'))
# any_updated <- FALSE
# lapply(lockfile, function(pkg) {
#   if (packageVersion(pkg$name) != pkg$version) {
#     if (!any_updated) {
#       packageStartupMessage(crayon::yellow(
#         "Packages are out of date, updating...\n"))
#     }
#     cat(crayon::red(paste0(
#       pkg$name, " is out of date, re-installing...\n")))
#     get_ref <- function(pkg) {
#       if (is.null(pkg$ref)) { pkg$version } else { pkg$ref }
#     }
#     devtools::install_github(pkg$repo, ref = get_ref(pkg))
#     any_updated <<- TRUE
#   }
# })
# 
# if (any_updated) {
#   packageStartupMessage(crayon::yellow(
#     "Packages have been updated, exiting R. Please restart...\n"))
#   base::q(save = 'no'); stop('Quit R') # Force quit R
# }
