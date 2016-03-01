## Borrowed from tools::package.dependencies and modified, adding LinkingTo and Remotes
## to depLevel and allowing for multiple depLevels to be passed in simultaneously
parse_dcf <- function (x, check = FALSE, depLevel = c("Depends", "Imports",
  "Suggests", "LinkingTo", "Remotes")) {
  depLevel <- match.arg(depLevel, several.ok = TRUE)
  if (!is.matrix(x))
      x <- matrix(x, nrow = 1L, dimnames = list(NULL, names(x)))
  if (!any(colnames(x) %in% depLevel)) return(NULL)
  deps <- list()
  for (k in 1L:nrow(x)) {
    z <- x[k, colnames(x) %in% depLevel]
    if (all(!is.na(z)) & all(z != "")) {
        z <- unlist(strsplit(z, ",", fixed = TRUE))
        z <- sub("^[[:space:]]*(.*)", "\\1", z)
        z <- sub("(.*)[[:space:]]*$", "\\1", z)
        pat <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
        deps[[k]] <- cbind(sub(pat, "\\1", z), sub(pat, "\\2",
            z), NA)
        noversion <- deps[[k]][, 1] == deps[[k]][, 2]
        deps[[k]][noversion, 2] <- NA
        pat <- "[[:space:]]*([[<>=]+)[[:space:]]+(.*)"
        deps[[k]][!noversion, 2:3] <- c(sub(pat, "\\1", deps[[k]][!noversion,
            2]), sub(pat, "\\2", deps[[k]][!noversion, 2]))
    }
    else deps[[k]] <- NA
  }
  if (check) {
      z <- rep.int(TRUE, nrow(x))
      for (k in 1L:nrow(x)) {
          if (!is.na(deps[[k]]) && any(ok <- deps[[k]][, 1] ==
              "R")) {
              if (!is.na(deps[[k]][ok, 2]) && deps[[k]][ok,
                2] %in% c("<=", ">=")) {
                op <- deps[[k]][ok, 2]
                x1 <- rep.int(0, 6)
                y <- c(R.version$major, strsplit(R.version$minor,
                  ".", fixed = TRUE)[[1L]])
                x1[seq_along(y)] <- y
                y <- strsplit(deps[[k]][ok, 3], ".", fixed = TRUE)[[1L]]
                x1[3 + seq_along(y)] <- y
                x1 <- format(x1, justify = "right")
                x2 <- paste(x1[4:6], collapse = ".")
                x1 <- paste(x1[1L:3], collapse = ".")
                comptext <- paste0("'", x1, "' ", op, " '",
                  x2, "'")
                compres <- try(eval(parse(text = comptext)))
                if (!inherits(compres, "try-error")) {
                  z[k] <- compres
                }
              }
          }
      }
      names(z) <- x[, "Package"]
      return(z)
  }
  else {
      names(deps) <- x[, "Package"]
      return(deps)
  }
}
