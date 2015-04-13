.lockbox_env <- new.env()

set_transient_library <- function() {
  if (!is.null(.lockbox_env$old_dir)) return()

  dir <- lockbox_transient_dir()
  if (!file.exists(dir)) dir.create(dir, FALSE, TRUE)
  .lockbox_env$old_dir <- .libPaths()
  .libPaths(dir)
}

set_default_mirror <- function() {
  # Set default CRAN mirror to Rstudio's unless the user requests not to.
  if (is.null(getOption("lockbox.disable_default_mirror"))) {
    if (is.null(getOption("repos"))) {
      .lockbox_env$old_opts <- 
        options(repos = structure(c(CRAN = "http://cran.rstudio.com/")))
    }
  }
}

.onLoad <- function(pkg, libPath) {
  set_transient_library()
}

.onUnLoad <- function(pkg) {
  .libPaths(.lockbox_env$old_dir)
  if (exists("old_opts", envir = .lockbox_env, inherits = FALSE)) {
    options(.lockbox_env$old_opts)
  }
}

