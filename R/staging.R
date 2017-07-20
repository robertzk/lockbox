#' Construct a lockbox staging library.
#'
#' To install a new package to the lockbox library, a virtual library is
#' constructed that is used during the installation process. This
#' function generates that library and returns its (temporary)
#' directory name. See the vignette on "Libraries in Lockbox" for more details.
#'
#' @return the directory of the staging library. This should be unlinked 
#'   after use.
staging_library <- function() {
  # We need to be careful to pick a directory the user will always
  # have access to. The only guarantees we have are the lockbox directory
  # and lockbox transient library themselves.
  tempdir <- file.path(lockbox_library(), ".staging")
  dir.create(tempdir, FALSE, TRUE)  
  tempdir <- normalizePath(tempdir)

  # Iterate over libraries in reverse order so that libraries with overlapping
  # packages get the relevant version of the package symlinked.
  lapply(rev(.libPaths()), symlink_library, tempdir)

  tempdir
}

#' Symlink all packages from a destination library to a target library.
#'
#' @param src character. The location of the library to use for
#'   generating symlinks
#' @param target character. The location of the library which will be
#'   populated with symlinked versions of the packages in the \code{destination}
#'   library.
symlink_library <- function(src, target) {
  packages <- list.files(src, full.names = TRUE)
  if (length(packages)) {
    new_packages <- file.path(target, basename(packages))
    Map(symlink, packages, new_packages, force = TRUE)
  }
}

#' Copy real (non-symlinked) packages to the lockbox library.
#'
#' @param staging_library character. The location of the staging library.
copy_real_packages_to_lockbox_library <- function(staging_library) {
  with_real_packages(staging_library, move_package_to_lockbox_library)
}

move_package_to_lockbox_library <- function(pkg_path) {
  tmp_path <- file.path(lockbox_library(), basename(pkg_path))
  new_path <- file.path(tmp_path, package_version_from_path(pkg_path), basename(pkg_path))
  dir.create(dirname(new_path), FALSE, TRUE)
  unlink(new_path, TRUE, TRUE)
  file.rename(pkg_path, new_path)
}

with_real_packages <- function(libpath, action) {
  stopifnot(is.function(action))

  packages <- list.files(libpath, full.names = TRUE)
  packages <- packages[!is.symlink(packages)]

  for (i in seq_along(packages)) {
    action(packages[i])
  }
}


