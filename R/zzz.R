.lockbox_env <- new.env()

.onLoad <- function(pkg, libPath) {
  dir <- lockbox_transient_dir()
  if (!file.exists(dir)) dir.create(dir, FALSE, TRUE)
  .lockbox_env$old_dir <- .libPaths()[1L]
  .libPaths(dir)
}

.onUnLoad <- function(pkg) {
  .libPaths(.lockbox_env$old_dir)
}

