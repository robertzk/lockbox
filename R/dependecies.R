#' Get dependencies for all elements with lock, but only do so for current
#' version mismatches and non-cran installations
get_ordered_dependencies <- function(lock, mismatches) {
   cat(crayon::blue(paste("Retrieving dependency info...")))
   deps <- get_dependencies_for_list(lock[mismatches], lock, list(), "")
   cat("\n")
   deps
}

#' Recursive function to take a list and lock and extract dependencies, sorting
#' along the way using the combine_dependencies function.
get_dependencies_for_list <- function(master_list, lock, previously_parsed_deps, current_parent) {
  current_dependencies <- master_list
  for (i in 1:length(master_list)) {
    package <- master_list[[i]]
    if (!is_previously_parsed(package, previously_parsed_deps)) {
      single_package_dependencies <- get_dependencies(
       structure(package
         , class = c(package$remote %||% "CRAN"
           , class(package))))
      previously_parsed_deps[[length(previously_parsed_deps) + 1]] <- list(
        package = package
        , dependencies = single_package_dependencies)
    } else{
      single_package_dependencies <- previously_parsed_deps[[which_previously_parsed(
        package, previously_parsed_deps)]][["dependencies"]]
    }
    current_dependencies <- combine_dependencies(
      single_package_dependencies
      , current_dependencies
      , package$name)
  }
  current_list <- add_details(current_dependencies, lock)
  current_list <- lapply(current_list, as.dependency_package)
  current_list <- reset_to_locked(current_list, lock)
  if (identical(master_list, current_list)) return(master_list)
  get_dependencies_for_list(current_list, lock, previously_parsed_deps, current_parent)
}

#' Have we previously gotten this packages dependencies?
is_previously_parsed <- function(package, previously_parsed_deps) {
  location <- which_previously_parsed(package, previously_parsed_deps)
  if (identical(location, 0L)) {
    FALSE
  } else{
    TRUE
  }
}

#' Where, if anywhere, have I stored this packages/version combinations'
#' previously parsed dependencies?
which_previously_parsed <- function(package, previously_parsed_deps) {
  if (length(previously_parsed_deps) == 0) return(0L)
  previously_parsed_names <- vapply(previously_parsed_deps
    , function(p) p[["package"]]$name, character(1))
  if (package$name %in% previously_parsed_names) {
    sel <- which(previously_parsed_names == package$name)
    subsel <- vapply(previously_parsed_deps[sel]
      , function(p) identical(p[["package"]]$version, package$version)
      , logical(1))
    if (any(subsel)) {
      return(sel[subsel])
    }
  }
  0L
}

#' Check a dependency list for inclusion in the lockfile and add those additional
#' details (repo, remote, version, subdir) if it does appear there.  Also throw
#' an error if we require a dependency version greater than that specified by
#' the lockfile.
add_details <- function(current_list, lock) {
  lock_names <- vapply(lock, function(l) l$name, character(1))
  lapply(current_list
    , function(el) {
      if (el$name %in% lock_names) {
        locked_package <- lock[[which(lock_names == el$name)[1]]]
        if (is.na(el$version) || package_version(as.character(el$version)) <
          package_version(as.character(locked_package$version))) {
            el$version <- as.character(locked_package$version)
        }
        if (package_version(as.character(el$version)) >
          package_version(as.character(locked_package$version))) {
            stop(paste0("Dependency: \'", el$name, ", Version: ", el$version
              , " is required, but lockbox is locked at version: "
              , as.character(locked_package$version)))
        }
        fields_to_replace <- c("ref", "repo", "remote", "subdir", "dir", "load")
        invisible(lapply(fields_to_replace, function(field) {
          if (field %in% names(locked_package)) {
            el[[field]] <<- locked_package[[field]]
          }}))
      }
      if (!"remote" %in% names(el) || is.na(el$remote)) {
        el$remote <- "CRAN"
      }
      el})
}

#' Combine two lists of dependencies via version comparisons.  Keep packages
#' found in list1 on the left side of the entirety of list2, while moving
#' the parent package to the space after it's rightmost dependency found in list2.
#' Update versions with greatest values if necessary.
combine_dependencies <- function(list1, list2, current_parent) {
  names1 <- vapply(list1, function(obj) obj$name, character(1))
  names2 <- vapply(list2, function(obj) obj$name, character(1))

  version1 <- vapply(list1, function(obj) as.character(obj$version), character(1))
  version2 <- vapply(list2, function(obj) as.character(obj$version), character(1))

  remote1 <- vapply(list1
    , function(obj) if(is.null(obj$remote)) as.character(NA) else as.character(obj$remote)
    , character(1))
  remote2 <- vapply(list2
    , function(obj) if(is.null(obj$remote)) as.character(NA) else as.character(obj$remote)
    , character(1))

  # Certain packages are no longer on cran but incorporated into R Core
  core_pkgs <- as.character(installed.packages(priority = "base")[,1])
  version1 <- version1[!names1 %in% core_pkgs]
  remote1 <- remote1[!names1 %in% core_pkgs]
  list1 <- list1[!names1 %in% core_pkgs]
  names1 <- names1[!names1 %in% core_pkgs]
  version2 <- version2[!names2 %in% core_pkgs]
  remote2 <- remote2[!names2 %in% core_pkgs]
  list2 <- list2[!names2 %in% core_pkgs]
  names2 <- names2[!names2 %in% core_pkgs]

  if (length(list1) == 0) return(list2)
  if (length(list2) == 0) return(list1)

  names(version1) <- names1
  names(version2) <- names2
  keep1 <- !names1 %in% names2

  if (current_parent %in% names2 && any(names2 %in% names1)) {
    new_selection <- !names1 %in% names2
    init_parent_slot <- which(names2 == current_parent)
    final_parent_slot <- max(which(names2 %in% names1))
    if (final_parent_slot > init_parent_slot) {
      sel1 <- seq_along(names2) != init_parent_slot & seq_along(names2) <= final_parent_slot
      sel2 <- seq_along(names2) > final_parent_slot
      version2 <- c(version2[sel1], version2[init_parent_slot], version2[sel2])
      remote2 <- c(remote2[sel1], remote2[init_parent_slot], remote2[sel2])
      names2 <- c(names2[sel1], names2[init_parent_slot], names2[sel2])
    }
  }

  swap <- swap_versions(names1, names2, version1, version2, remote1, remote2)
  version2 <- swap[["version"]]
  remote2 <- swap[["remote"]]

  names_final <- c(names1[keep1], names2)
  version_final <- c(version1[keep1], version2)
  remote_final <- c(remote1[keep1], remote2)
  Map(function(n, v, r) list(name = n, version = v, remote = r)
    , names_final, version_final, remote_final)
}

#' Swap versions information when side1 is greater than side2
swap_versions <- function(names1, names2, version1, version2, remote1, remote2) {
  swap_version2for1 <- vapply(
    names1
    , function(n) {
      if (n %in% names2) {
        if (is.na(version2[[n]])) return(TRUE)
        if (is.na(version1[[n]])) return(FALSE)
        package_version(version1[[n]]) >= package_version(version2[[n]])
      } else{
        FALSE
      }}
    , logical(1))
  swap_versions1  <- names1 %in% names2 & swap_version2for1
  swap_versions2 <- vapply(names1[swap_versions1]
    , function(n) which(names2 == n)
    , integer(1))
  version2[swap_versions2] <- version1[swap_versions1]
  remote2[swap_versions2] <- remote1[swap_versions1]
  list(version = version2, remote = remote2)
}

#' Either use the current library DESCRIPTION
#' file or download the accurate remote DESCRIPTION file.
get_dependencies <- function(package) {
  is_local_dependency <- is.dependency_package(package) &&
    !is.na(current_version(package)) &&
    (is.na(package$version) ||
      package_version(as.character(current_version(package))) <=
      package_version(as.character(package$version)))
  if (is_local_dependency) {
    dependencies_from_description(package, description_file_for(package$name, .libPaths()[3L]))
  } else {
    cat(crayon::blue("."))
    output = tryCatch(get_remote_dependencies(package), error = function(e) e)
    if(is(output, "error")) {
      message(crayon::red(paste0("Dependencies could not be resolved for package: "
        , package$name, " version: ", package$version)))
      list()
    } else {
      output
    }
  }
}

get_remote_dependencies <- function(package) {
  UseMethod("get_remote_dependencies")
}

#' If a package is local we just read from the directory given
get_remote_dependencies.local <- function(package) {
  description_name <- file_list$Name[grepl(paste0("^[^/]+"
    ,"/DESCRIPTION$"), file_list$Name)]
  dcf <- read.dcf(description_name)
  dependencies_from_description(package, dcf)
}

#' For packages on CRAN we will extract to a temporary directory when we
#' download the accurate remote DESCRIPTION file.
get_remote_dependencies.CRAN <- function(package) {
  remote <- package$remote
  filepath <- download_package(structure(
    package,
    class = c(remote, class(package))))
  split_fp <- strsplit(filepath, "/")[[1]]
  dirpath <- paste(split_fp[-length(split_fp)], collapse = "/")
  file_list <- untar(filepath, list = TRUE)
  description_name <- file_list[grepl(paste0("^[^/]+"
    ,"/DESCRIPTION$"), file_list)]
  untar(filepath, description_name, exdir = dirpath)
  description_path <- paste0(dirpath, "/", description_name)
  dcf <- read.dcf(file = description_path)
  unlink(filepath)
  dependencies_from_description(package, dcf)
}

#' Download the accurate remote DESCRIPTION file for a github repo.
get_remote_dependencies.github <- function(package) {
  dcf <- download_description_github(package)
  dependencies_from_description(package, dcf)
}

download_description_github <- function(package) {
  remote <- package$remote
  filepath <- download_package(structure(
    package,
    class = c(remote, class(package))))
  file_list <- unzip(filepath, list = TRUE)
  subdir <- ""
  if (!is.null(package$subdir)){
    subdir <- paste0("/",package$subdir)
  }
  description_name <- file_list$Name[grepl(paste0("^[^/]+"
    , subdir
    ,"/DESCRIPTION$"), file_list$Name)]
  file_con <- unz(filepath, description_name)
  dcf <- read.dcf(file = file_con)
  close(file_con)
  unlink(filepath)
  dcf
}

version_from_remote <- function(package) {
  dcf <- download_description_github(package)
  version_from_description(package, dcf)
}

version_from_description <- function(package_name, dcf) {
  browser()
  if (!"Version" %in% colnames(dcf)) return(NA)
  dcf[["Version"]]
}

#' Parse dependencies from description using the tools package
dependencies_from_description <- function(package, dcf) {
  dependency_levels <- c("Depends", "Imports", "Remotes")
  dependency_levels %in% colnames(dcf)
  if (!any(dependency_levels %in% colnames(dcf))) return(list())
  dependencies_parsed <- as.data.frame(parse_dcf(dcf
    , depLevel = dependency_levels[dependency_levels %in%
      colnames(dcf)])[[package$name]])
  non_remote_dependencies <- dependencies_parsed[rownames(dependencies_parsed) != "Remotes", ]
  remote_dependencies <- dependencies_parsed[rownames(dependencies_parsed) == "Remotes", ]
  non_remote_dependencies <- non_remote_dependencies[!grepl("^[rR]$"
    , non_remote_dependencies[,1]), , drop = FALSE]
  if (identical(nrow(non_remote_dependencies),0L)){
    non_remote_list <- list()
  } else {
    non_remote_list <- lapply(seq_along(non_remote_dependencies[,1])
      , function(i) {
        list(name = as.character(non_remote_dependencies[i,1])
          , version = as.character(non_remote_dependencies[i,3]))
      })
  }
  if (identical(nrow(remote_dependencies),0L)){
    remote_list <- list()
  } else {
    matches_github <- grepl("github::"
      , remote_dependencies[,1])
    matches_unsupported <- grepl("bitbucket::|svn::|url::|local::|gitorious"
      , remote_dependencies[,1])
    if (any(matches_unsupported)) {
      remote_list <- list()
    } else{
      remote_list <- lapply(seq_along(remote_dependencies[,1])
        , function(i) {
          name <- remote_dependencies[i,1]
          if (matches_github[i]){
            name <- gsub("git::.*github\\.com/", "", name)
            name <- gsub("\\.git", "", name)
          }
          pkg <- list(name = as.character(remote_dependencies[i,1])
            , version = as.character(remote_dependencies[i,3])
            , remote = "github")
          if (is.na(pkg$version)) {
            pkg$version <- version_from_remote(
             structure(
               pkg
               , class = c(pkg$remote %||% "CRAN" , "dependency_package")))
          }
          pkg
        })
    }
  }
  c(non_remote_list, remote_list)
}

download_package <- function(package) {
  UseMethod("download_package")
}

#' Download CRAN package, either current or older version
download_package.CRAN <- function(package) {
  remote_version <- get_available_cran_version(package)
  name <- package$name
  version <- package$version
  repo = "http://cran.r-project.org"
  if (!is.locked_package(package)) {
    version <- NA
  }

  ###Some packages are available in archive only
  if (is.na(remote_version)) {
    archive_addition <- paste0("Archive/", name, "/")
    url <- paste0(repo, "/src/contrib/", archive_addition)
    filenames <- RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
    filenames <- paste(url, strsplit(filenames, "\r*\n")[[1]], sep = "")
    filenames <- filenames[grepl(paste0(name, "_[0-9\\.\\-]+\\.tar\\.gz"),filenames)]
    expr <- gregexpr(paste0(name, "_([0-9\\.\\-]+)\\.tar\\.gz"), filenames)
    filenames <- vapply(regmatches(filenames, expr), function(match) match[1], character(1))
    archived_version <- gsub("(.*_)([0-9\\.\\-]+)(\\.tar\\.gz)", "\\2", filenames[length(filenames)])
    if (is.na(version) || package_version(archived_version) > package_version(as.character(version))) {
      version <- archived_version
    }
  } else{
    # Simply download latest if version happens to be the latest available on CRAN.
    remote_version <- as.character(remote_version)
    if (is.na(version) || package_version(remote_version) == package_version(version)) {
      version <- remote_version
      archive_addition <- ""
    } else{
      archive_addition <- paste0("Archive/", name, "/")
    }
  }

  from <- paste0(repo, "/src/contrib/", archive_addition, name, "_", version, ".tar.gz")
  pkg_tarball <- tempfile(fileext = ".tar.gz")
  out <- suppressWarnings(tryCatch(
    download.file(url = from, destfile = pkg_tarball, quiet = notTRUE(getOption('lockbox.verbose')))
    , error = function(e) e))
  # Sometimes the current version isn't accessible in it's usual place, but is already archived
  if (is(out, "error")) {
    archive_addition <- paste0("Archive/", name, "/")
    from <- paste0(repo, "/src/contrib/", archive_addition, name, "_", version, ".tar.gz")
    download.file(url = from, destfile = pkg_tarball, quiet = notTRUE(getOption('lockbox.verbose')))
  }
  pkg_tarball
}

get_available_cran_version <- function(package, repo = "http://cran.r-project.org") {
  repo = "http://cran.r-project.org"

  # List available packages on the repo
  available <- available.packages(contriburl =
    contrib.url(repos = "http://cran.us.r-project.org", type = "source"))
  available <- data.frame(unique(available[, c("Package", "Version")]))
  pkg <- available[available$Package == package$name, ]

  if (nrow(pkg) == 0) {
    NA
  } else{
    pkg$Version
  }
}

#' Download a package from github using devtools' remote_download function
download_package.github <- function(package) {
  remote <- get_remote(package)
  if (is.na(remote$version)) {
    remote$version <- NULL
  }
  quiet <- !isTRUE(getOption('lockbox.verbose'))
  devtools:::remote_download.github_remote(remote, quiet = quiet)
}

#' Create remote in form devtools' remote_download likes.
get_remote <- function(package) {
  ref <- package$ref %||% package$version
  if (is.null(ref)) {
    arguments <- list(package$repo)
  } else {
    arguments <- list(
      paste(package$repo, ref, sep = "@"))
  }
  if (nzchar(token <- Sys.getenv("GITHUB_PAT"))) {
    arguments$auth_token <- token
  }
  if (!is.null(package$subdir)) {
    arguments$subdir <- package$subdir
  }
  remote <- do.call(devtools:::github_remote, arguments)
}

#' Borrowed from tools::package.dependencies and modified to be less breaky
#' and parse remotes
parse_dcf <- function (x, check = FALSE, depLevel = c("Depends", "Imports",
  "Suggests", "Remotes")) {
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

