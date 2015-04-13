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
  tempdir <- file.path(tempdir(), "staging")
  dir.create(tempdir, FALSE, TRUE)  

  # Iterate over libraries in reverse order so that libraries with overlapping
  # packages get the relevant version of the package symlinked.
  lapply(rev(.libPaths()), symlink_library, tempdir)

  tempdir
}

#' Symlink all packages from a destination library to a target library.
#'
#' @param destination character. The location of the library to use for
#'   generating symlinks
#' @param target character. The location of the library which will be
#'   populated with symlinked versions of the packages in the \code{destination}
#'   library.
symlink_library <- function(destination, target) {
  packages     <- list.files(destination, full.names = TRUE)
  new_packages <- file.path(target, basename(packages))
  Map(symlink, packages, new_packages, force = TRUE)
}

#' Copy real (non-symlinked) packages to the lockbox library.
#'
#' @param staging_library character. The location of the staging library.
copy_real_packages_to_lockbox_library <- function(staging_library) {
  packages <- list.files(staging_library, full.names = TRUE)
  packages <- Filter(Negate(is.symlink), packages)
  lapply(packages, move_package_to_lockbox_library)
}

move_package_to_lockbox_library <- function(pkg_path) {
  tmp_path <- file.path(lockbox_library(), basename(pkg_path))
  new_path <- file.path(tmp_path, package_version_from_path(pkg_path))
  # A little gymnastics because R sucks at copying directories.
  dir.create(tmp_path, FALSE, TRUE)
  file.copy(pkg_path, tmp_path, TRUE, TRUE)
  unlink(new_path, TRUE, TRUE)
  file.rename(file.path(tmp_path, basename(pkg_path)), new_path)
}

