crayon_blue <- function(string) {
  paste0("\033[34m", string,"\033[39m")
}

crayon_red <- function(string) {
  paste0("\033[31m", string,"\033[39m")
}

crayon_yellow <- function(string) {
  paste0("\033[33m", string,"\033[39m")
}

crayon_green <- function(string) {
  paste0("\033[32m", string,"\033[39m")
}
