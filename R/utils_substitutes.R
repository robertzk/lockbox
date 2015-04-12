#' @export
setNames <- function(object = nm, nm) {
  names(object) <- nm
  object
}

#' @export
packageVersion <- function (pkg, lib.loc = NULL) {
  res <- suppressWarnings(utils::packageDescription(pkg, lib.loc = lib.loc, fields = "Version"))
  if (!is.na(res))
    base::package_version(res)
  else stop(gettextf("package %s not found", sQuote(pkg)), domain = NA)
}

#' @export
untar <- function(...) {
  utils::untar(...)
}
