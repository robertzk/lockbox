.lockbox_env <- new.env()

set_transient_library <- function() {
  if (!is.null(.lockbox_env$old_dir)) return()

  dir <- lockbox_transient_dir()
  if (!file.exists(dir)) dir.create(dir, FALSE, TRUE)
  .lockbox_env$old_dir <- .libPaths()[1L]
  .libPaths(dir)
}

.onLoad <- function(pkg, libPath) {
  set_transient_library()
}

.onUnLoad <- function(pkg) {
  .libPaths(.lockbox_env$old_dir)
}

