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


}

#' Symlink all packages from a destination library to a target library.
#'
#' @param destination character. The location of the library to use for
#'   generating symlinks
#' @param target character. The location of the library which will be
#'   populated with symlinked versions of the packages in the \code{destination}
#'   library.
symlink_library <- function(destination, target) {
}
