setNames <- function(object = nm, nm) {
  names(object) <- nm
  object
}

packageVersion <- function (pkg, lib.loc = NULL) {
  res <- suppressWarnings(utils::packageDescription(pkg, lib.loc = lib.loc, fields = "Version"))
  if (!is.na(res)) {
    base::package_version(res)
  } else {
    stop(gettextf("package %s not found", sQuote(pkg)), domain = NA)
  }
}

untar <- function(...) {
  utils::untar(...)
}

