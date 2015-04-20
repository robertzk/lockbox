#' Remove all packages from lockbox `library`.
#'
#' @rdname cleanup
#' @export
emptybox <- function() {
  quietly(unlink(lockbox_library()))
}
