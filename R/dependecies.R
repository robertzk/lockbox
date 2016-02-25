#' Get dependencies for all elements in lock
get_ordered_dependencies <- function(lock) {
   cat(crayon::blue(paste("Retrieving dependency info...")))
   deps <- get_dependencies_for_list(lock, lock, list())
   cat("\n")
   deps
}

#' Recursive function to take a list and lock and extract dependencies, sorting
#' along the way using the combine_dependencies function.
#' @param master_list list.  Packages to go through and parse dependencies from
#' @param lock list.  Original list of locked packages
#' @param previously_parsed_deps.  List of packages and their dependencies that
#'   we have already parsed out of their respective description files
get_dependencies_for_list <- function(master_list, lock, previously_parsed_deps) {
  ## Start off with the master list as our set of packages
  current_dependencies <- master_list
  for (i in seq_along(master_list)) {
    package <- master_list[[i]]

    ## Find out if we've parsed this package's description before and, if so,
    ## where we've stored it's list of dependencies in our humongous
    ## previously_parsed_deps list object.  If this comes back as 0 then we
    ## have not parsed it yet
    previously_parsed_loc <- which_previously_parsed(
        package, previously_parsed_deps)
    if (identical(previously_parsed_loc, 0L)) {
      single_package_dependencies <- get_dependencies(
       structure(package
         , class = c(package$remote %||% "CRAN"
           , class(package)))
       , lock)
      ## Store the dependencies for this particular package in our humongous
      ## previously_parsed_deps object
      previously_parsed_deps[[length(previously_parsed_deps) + 1]] <- list(
        package = package
        , dependencies = single_package_dependencies)
    } else {
      single_package_dependencies <-
        previously_parsed_deps[[previously_parsed_loc]]$dependencies
    }
    ## Now combine the dependencies from this package with our big dependency
    ## list
    current_dependencies <- combine_dependencies(
      single_package_dependencies
      , current_dependencies
      , package$name)
  }
  if (identical(master_list, current_dependencies)) return(master_list)

  ## Run through the entire list again, because we have a bunch of new packages
  get_dependencies_for_list(current_dependencies, lock, previously_parsed_deps)
}

#' Where, if anywhere, have I stored this packages/version combinations'
#' previously parsed dependencies?
which_previously_parsed <- function(package, previously_parsed_deps) {
  if (length(previously_parsed_deps) == 0) return(0L)
  previously_parsed_names <- vapply(previously_parsed_deps
    , function(p) p$package$name, character(1))
  if (package$name %in% previously_parsed_names) {
    sel <- which(previously_parsed_names == package$name)
    subsel <- vapply(previously_parsed_deps[sel]
      , function(p) identical(p$package$version, package$version)
      , logical(1))
    if (any(subsel)) {
      return(sel[subsel])
    }
  }
  0L
}

#' Attach the latest available lockbox version to a package
add_latest_version_in_lockbox <- function(package) {
  package$latest_version_in_lockbox <- max_package_version(
    list.files(file.path(lockbox_library(), package)))
  package
}

#' Take a vector of versions and find the max without coercing to package_version
max_package_version <- function(versions) {
  if (length(versions) == 0) return(NULL)
  formatted_versions <- package_version(as.character(versions))
  versions[which(formatted_versions == max(formatted_versions))[1]]
}

#' Check a dependency list for inclusion in the lockfile and replace the package
#' with the locked version if it does appear there.  Also throw
#' an error if we require a dependency version greater than that specified by
#' the lockfile.
replace_with_lock <- function(package, lock) {
  lock_names <- vapply(lock, function(l) l$name, character(1))
  if (package$name %in% lock_names) {
    locked_package <- Filter(function(l) l$name == package$name, lock)[[1]]
    if (!is.na(package$version) && package_version(as.character(package$version)) >
      package_version(as.character(locked_package$version))) {
        stop(paste0("Dependency: \'", package$name, ", Version: ", package$version
          , " is required by package ", package$parent_package
          , ", but lockbox is locked at version: "
          , as.character(locked_package$version)
          , ". Please update your lockfile accordingly"))
    }
    package <- locked_package
  } else {
    package$is_dependency_package <- TRUE
  }
  if (!"remote" %in% names(package) || is.na(package$remote)) {
    package$remote <- "CRAN"
  }
  package <- as.locked_package(package)
  if (package$is_dependency_package && !package$name %in%
    as.character(installed.packages(priority = "base")[,1])) {
      package$latest_version <- package$latest_version_in_lockbox %||%
        get_latest_version(package)
  }
  package
}

get_latest_version <- function(package) {
  if (package$remote == "CRAN") {
    get_available_cran_version(package)
  } else {
    version_from_remote(package)
  }
}

#' Combine two lists of dependencies via version comparisons.  Keep packages
#' found in list1 on the left side of the entirety of list2, while moving
#' the parent package to the space after it's rightmost dependency found in list2.
combine_dependencies <- function(list1, list2, current_parent) {
  names <- lapply(list(list1, list2), function(lst) vapply(lst, `[[`, character(1), "name"))

  # Certain packages are no longer on cran but incorporated into R Core
  core_pkgs <- as.character(installed.packages(priority = "base")[,1])
  list1 <- list1[!names[[1]] %in% core_pkgs]
  names[[1]] <- names[[1]][!names[[1]] %in% core_pkgs]
  list2 <- list2[!names[[2]] %in% core_pkgs]
  names[[2]] <- names[[2]][!names[[2]] %in% core_pkgs]

  if (length(list1) == 0) return(list2)
  if (length(list2) == 0) return(list1)

  names(list1) <- names[[1]]
  names(list2) <- names[[2]]

  ## Find the rightmost dependency of the current package and move our current
  ## package to the immediate right of that spot.  This preserves previous
  ## sorting order while ensuring that the current package will be installed
  ## after all its dependencies
  if (current_parent %in% names[[2]] && any(names[[2]] %in% names[[1]])) {
    init_parent_slot <- which(names[[2]] == current_parent)
    final_parent_slot <- max(which(names[[2]] %in% names[[1]]))
    if (final_parent_slot > init_parent_slot) {
      sel1 <- seq_along(names[[2]]) != init_parent_slot & seq_along(names[[2]]) <= final_parent_slot
      sel2 <- seq_along(names[[2]]) > final_parent_slot
      list2 <- c(list2[sel1], list2[init_parent_slot], list2[sel2])
      names[[2]] <- c(names[[2]][sel1], names[[2]][init_parent_slot], names[[2]][sel2])
    }
  }

  list2 <- swap_packages(names[[1]], names[[2]], list1, list2)

  c(list1[!names[[1]] %in% names[[2]]], list2)
}

#' Swap packages by comparing version information. 1ist2 has already
#' been ordered by dependency, so is imperative that we keep its order while
#' potentially swapping in the corresponding packages in list1 that have later
#' version requirements.
swap_packages <- function(names1, names2, list1, list2) {
  ## Swap packages when the package in list1 is more recent (> version) than
  ## it's corresponding package in list2.  Keep the list 2 element if it is a
  ## locked package, as well.  If their remotes are not identical and neither
  ## has specified a version, then keep whichever remote has a more recent version
  swap_package2for1 <- vapply(
    names1
    , function(n) {
      if (!n %in% names2) return(FALSE)
      obj1 <- list1[[n]]
      obj2 <- list2[[n]]
      if (obj1$is_dependency_package && obj2$is_dependency_package) {
        if (is.na(obj1$version) && is.na(obj2$version)) {
          (obj1$remote != obj2$remote) &&
            package_version(obj1$latest_version) >
            package_version(obj2$latest_version)
        } else {
          !is.na(obj1$version) && (is.na(obj2$version) ||
            package_version(obj1$version) > package_version(obj2$version))
        }
      } else {
        obj2$is_dependency_package
      }
    }
    , logical(1))

  ## To swap a package from list1 into list2
  list2_swap <- vapply(names1[swap_package2for1]
    , function(n) which(names2 == n)
    , integer(1))
  list2[list2_swap] <- list1[swap_package2for1]
  list2
}

#' Either use the current lockbox library DESCRIPTION
#' file or download the accurate remote DESCRIPTION file.
get_dependencies <- function(package, lock) {
  locked_package <- package

  ## When we have a dependency package that has a version in lockbox
  ## we will substitute that version for our package version if that package
  ## version is missing (meaning use any version available) or this required
  ## version is less than that already in the lockbox
  if (locked_package$is_dependency_package &&
    !is.null(locked_package$latest_version_in_lockbox) &&
    (is.na(locked_package$version) ||
      package_version(as.character(locked_package$version)) <
      package_version(as.character(locked_package$latest_version_in_lockbox)))) {
      locked_package$version <- locked_package$latest_version_in_lockbox
  }
  if (!is.na(locked_package$version) && exists_in_lockbox(locked_package)) {
    dependencies <- dependencies_from_description(locked_package
      , description_file_for(locked_package$name
        , gsub("/[^/]+$", "", lockbox_package_path(locked_package))))
  } else {
    cat(crayon::blue("."))
    output <- tryCatch(get_remote_dependencies(package), error = function(e) e)
    if (is(output, "error")) {
      message(crayon::red(paste0("Dependencies could not be resolved for package: "
        , package$name, " version: ", package$version)))
      dependencies <- list()
    } else {
      dependencies <- output
    }
  }
  dependencies <- strip_available_dependencies(dependencies)
  dependencies <- lapply(dependencies, add_latest_version_in_lockbox)
  dependencies <- lapply(dependencies, function(dep) {
    dep$parent_package <- package$name
    dep})
  lapply(dependencies, replace_with_lock, lock)
}

strip_available_dependencies <- function(dependencies) {
  dependencies <- lapply(dependencies
    , function(package) {
      if(package$name %in% pesky_namespaces) NULL
      else package
    })
 dependencies[!vapply(dependencies, is.null, logical(1))]
}

#' Get the dependencies for a given package
get_remote_dependencies <- function(package) {
  UseMethod("get_remote_dependencies")
}

#' If a package is local we just read from the directory given
get_remote_dependencies.local <- function(package) {
  description_name <- file_list$Name[grepl(paste0("^[^/]+"
    ,"/DESCRIPTION$"), file_list$Name)]
  dependencies_from_description(package, read.dcf(description_name))
}

#' For packages on CRAN we will extract to a temporary directory when we
#' download the accurate remote DESCRIPTION file. Because these are tarballs
#' there is no simple way to extract only our desired file like we can with
#' zipfiles using the unz function.
get_remote_dependencies.CRAN <- function(package) {
  if (package$is_dependency_package) {
    package$version <- NA
  }
  filepath <- download_package(package)
  split_fp <- strsplit(filepath, "/")[[1]]
  dirpath <- dirname(filepath)
  file_list <- untar(filepath, list = TRUE)
  description_name <- file_list[grepl(paste0("^[^/]+"
    ,"/DESCRIPTION$"), file_list)]
  output <- untar(filepath, description_name, exdir = dirpath)
  description_path <- paste0(dirpath, "/", description_name)
  unlink(filepath)
  dcf <- read.dcf(file = description_path)
  unlink(description_path)
  dependencies_from_description(package, dcf)
}

#' Download the accurate remote DESCRIPTION file for a github repo.
get_remote_dependencies.github <- function(package) {
  dependencies_from_description(package, download_description_github(package))
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
  version_from_description(package, download_description_github(package))
}

version_from_description <- function(package_name, dcf) {
  if (!"Version" %in% colnames(dcf)) return(NA)
  as.character(dcf[1, which(colnames(dcf) == "Version")])
}

#' Parse dependencies from description
dependencies_from_description <- function(package, dcf) {
  ## We install 3 kinds of dependencies listed in the description file. If our
  ## dcf does not contain any of these elements we have no dependencies to
  ## speak of
  dependency_levels <- c("Depends", "Imports", "Remotes")
  if (!any(dependency_levels %in% colnames(dcf))) return(list())

  ## The parse_dcf function returns a matrix where rows correspond to packages
  ## and column 1 corresponds to the package name and column 3 corresponds to
  ## its version requirement.  The rownames of this matrix are the type of 
  ## dependency (Depends, Imports, or Remotes)
  dependencies_parsed <- as.data.frame(parse_dcf(dcf
    , depLevel = dependency_levels[dependency_levels %in%
      colnames(dcf)])[[package$name]])

  ## We separate out non-remote dependencies from remote dependencies, because
  ## they require different logic
  non_remote_dependencies <- dependencies_parsed[rownames(dependencies_parsed) != "Remotes", ]
  remote_dependencies <- dependencies_parsed[rownames(dependencies_parsed) == "Remotes", ]

  ## Remove the Depends entry that just corresponds to the R version 
  ## requirements
  non_remote_dependencies <- non_remote_dependencies[!grepl("^[rR]$"
    , non_remote_dependencies[,1]), , drop = FALSE]

  ## Parse the non-remote dependencies into a list of packages
  if (identical(nrow(non_remote_dependencies),0L)){
    non_remote_list <- list()
  } else {
    non_remote_list <- lapply(seq_along(non_remote_dependencies[,1])
      , function(i) {
        name <- as.character(non_remote_dependencies[i,1])
        version <- as.character(non_remote_dependencies[i,3])
        list(name = name, version = version)
      })
  }

  ## Parse the remote dependencies into a list of packages
  if (identical(nrow(remote_dependencies),0L)){
    remote_list <- list()
  } else {
    matches_github <- grepl("github::"
      , remote_dependencies[,1])

    ## We do not currently support non-github remotes
    matches_unsupported <- grepl("bitbucket::|svn::|url::|local::|gitorious"
      , remote_dependencies[,1])
    if (any(matches_unsupported)) {
      remote_list <- list()
    } else {
      remote_list <- lapply(seq_along(remote_dependencies[,1])
        , function(i) {
          name <- remote_dependencies[i,1]

          ## if github is expliitly stated as the remote then we remove
          ## such references
          if (matches_github[i]){
            name <- gsub("git::.*github\\.com/", "", name)
            name <- gsub("\\.git", "", name)
          }

          ## We extract the package name and repo name from the entry
          ## Could potentially fail if the repo is named something different 
          ## than its package name.  Other option is to download it now
          ## and parse it on the fly, but if we do that we have to do it
          ## every time we load, since we don't know it's name for package
          ## lookup in our lockbox directory
          subname <- gsub("^.*/", "", as.character(remote_dependencies[i,1]))
          subname <- gsub("@.*", "", subname)
          subrepo <- gsub("@.*", "", as.character(remote_dependencies[i,1]))
          pkg <- list(name = subname
            , repo = subrepo
            , version = as.character(remote_dependencies[i,3])
            , remote = "github")
          if (grepl("@", name)) {
            pkg$ref <- gsub(".*@", "", name)
          }
          pkg
        })
    }
  }

  ## Remote package names are duplicated in Depends and Imports entries
  if (length(remote_list) != 0) {
    non_remote_names <- vapply(non_remote_list, function(pkg) pkg$name, character(1))
    remote_names <- vapply(remote_list, function(pkg) pkg$name, character(1))
    if (any(non_remote_names %in% remote_names)) {
      non_remote_list <- non_remote_list[!non_remote_names %in% remote_names]
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
  repo <- "http://cran.r-project.org"
  if (package$is_dependency_package) {
    version <- NA
  }

  ## Simply download latest if version happens to be the latest available on CRAN.
  remote_version <- as.character(remote_version)
  if (is.na(version) || package_version(remote_version) == package_version(version)) {
    version <- remote_version
    archive_addition <- ""
  } else{
    archive_addition <- paste0("Archive/", name, "/")
  }

  from <- paste0(repo, "/src/contrib/", archive_addition, name, "_", version
    , ".tar.gz")
  pkg_tarball <- tempfile(fileext = ".tar.gz")
  out <- suppressWarnings(tryCatch(
    download.file(url = from, destfile = pkg_tarball
    , quiet = notTRUE(getOption('lockbox.verbose')))
      , error = function(e) e))
  ## Sometimes the current version isn't accessible in it's usual place
  ## , but is already archived
  if (is(out, "error")) {
    archive_addition <- paste0("Archive/", name, "/")
    from <- paste0(repo, "/src/contrib/", archive_addition, name, "_", version
      , ".tar.gz")
    download.file(url = from, destfile = pkg_tarball
      , quiet = notTRUE(getOption('lockbox.verbose')))
  }
  pkg_tarball
}

#' Download a package using the github remote arguments.  Borrowed entirely from
#' devtools
remote_download_github_remote <- function(x, quiet = FALSE) {
  dest <- tempfile(fileext = paste0(".zip"))
  src_root <- paste0("https://", x$host, "/repos/", x$username, "/", x$repo)
  src <- paste0(src_root, "/zipball/", x$ref)

  if (!quiet) {
    message("Downloading GitHub repo ", x$username, "/", x$repo, "@", x$ref,
            "\nfrom URL ", src)
  }

  if (!is.null(x$auth_token)) {
    auth <- httr::authenticate(
      user = x$auth_token,
      password = "x-oauth-basic",
      type = "basic"
    )
  } else {
    auth <- NULL
  }

  download(dest, src, auth)
}

#' Download a package from github our version of devtools' remote_download
#' function
download_package.github <- function(package) {
  if (is.na(package$version)) {
    package$version <- NULL
  }
  browser()
  remote <- get_remote(package)
  remote_download_github_remote(remote, quiet = !isTRUE(getOption('lockbox.verbose')))
}

#' Return the latest available version of a package from CRAN
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
  remote <- do.call(github_remote, arguments)
}

#' Get remote for a repo given a username, ref, subdir, auth_token, etc.  Stolen
#' shamelessly from devtools
github_remote <- function(repo, username = NULL, ref = NULL, subdir = NULL,
                       auth_token = github_pat(), sha = NULL,
                       host = "api.github.com") {

  meta <- parse_git_repo(repo)
  meta <- github_resolve_ref(meta$ref %||% ref, meta)

  if (is.null(meta$username)) {
    meta$username <- username %||% getOption("github.user") %||%
      stop("Unknown username.")
    warning("Username parameter is deprecated. Please use ",
      username, "/", repo, call. = FALSE)
  }

  remote("github",
    host = host,
    repo = meta$repo,
    subdir = meta$subdir %||% subdir,
    username = meta$username,
    ref = meta$ref,
    sha = sha,
    auth_token = auth_token
  )
}

#' Stolen from devtools.
#' Parse concise git repo specification: [username/]repo[/subdir][#pull|@ref|@*release]
#' (the *release suffix represents the latest release)
parse_git_repo <- function(path) {
  username_rx <- "(?:([^/]+)/)?"
  repo_rx <- "([^/@#]+)"
  subdir_rx <- "(?:/([^@#]*[^@#/]))?"
  ref_rx <- "(?:@([^*].*))"
  pull_rx <- "(?:#([0-9]+))"
  release_rx <- "(?:@([*]release))"
  ref_or_pull_or_release_rx <- sprintf("(?:%s|%s|%s)?", ref_rx, pull_rx, release_rx)
  github_rx <- sprintf("^(?:%s%s%s%s|(.*))$",
    username_rx, repo_rx, subdir_rx, ref_or_pull_or_release_rx)

  param_names <- c("username", "repo", "subdir", "ref", "pull", "release", "invalid")
  replace <- stats::setNames(sprintf("\\%d", seq_along(param_names)), param_names)
  params <- lapply(replace, function(r) gsub(github_rx, r, path, perl = TRUE))
  if (params$invalid != "")
    stop(sprintf("Invalid git repo: %s", path))
  params <- params[sapply(params, nchar) > 0]

  if (!is.null(params$pull)) {
    params$ref <- github_pull(params$pull)
    params$pull <- NULL
  }

  if (!is.null(params$release)) {
    params$ref <- github_release()
    params$release <- NULL
  }

  params
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

#' Resolve the ref.  Stolen from devtools
github_resolve_ref <- function(x, params) UseMethod("github_resolve_ref")

#' @export
github_resolve_ref.default <- function(x, params) {
  params$ref <- x
  params
}

#' @export
github_resolve_ref.NULL <- function(x, params) {
  params$ref <- "master"
  params
}

#' @export
github_resolve_ref.github_pull <- function(x, params) {
  # GET /repos/:user/:repo/pulls/:number
  path <- file.path("repos", params$username, params$repo, "pulls", x)
  response <- github_GET(path)

  params$username <- response$head$user$login
  params$ref <- response$head$ref
  params
}

# Retrieve the ref for the latest release
#' @export
github_resolve_ref.github_release <- function(x, params) {
  # GET /repos/:user/:repo/releases
  path <- paste("repos", params$username, params$repo, "releases", sep = "/")
  response <- github_GET(path)
  if (length(response) == 0L)
    stop("No releases found for repo ", params$username, "/", params$repo, ".")

  params$ref <- response[[1L]]$tag_name
  params
}
