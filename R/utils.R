`%||%` <- function(x, y) if (is.null(x)) y else x

#' @author Kevin Ushey
#' @source \url{https://github.com/rstudio/packrat/blob/92492ebc882bd048f092238af033d8a6fd03902f/R/utils.R#L469}
symlink <- function(from, to) {
  if (is.windows()) Sys.junction(from, to)
  else file.symlink(from, to)
}

#' @author Kevin Ushey
#' @source \url{https://github.com/rstudio/packrat/blob/ce9fb1de3ed490d3f85b0cae4534a3e998db659e/R/platform.R#L1}
is.windows <- function() {
  Sys.info()["sysname"] == "Windows"
}

#' @author Kevin Ushey
#' @source \url{https://github.com/rstudio/packrat/blob/ce9fb1de3ed490d3f85b0cae4534a3e998db659e/R/platform.R#L5}
is.mac <- function() {
  Sys.info()["sysname"] == "Darwin"
}

#' @author Kevin Ushey
#' @source \url{https://github.com/rstudio/packrat/blob/ce9fb1de3ed490d3f85b0cae4534a3e998db659e/R/platform.R#L9}
is.linux <- function() {
  Sys.info()["sysname"] == "Linux"
}

