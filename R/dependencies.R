#' Get dependencies for all elements in lock
#' @param lock list.  List of locked packages
get_ordered_dependencies <- function(lock) {
   cat(crayon_blue("Retrieving dependency info..."))
   names(lock) <- vapply(lock, `[[`, character(1), "name")
   package_list(lock, lock, list())
}

#' Recursive function to take a list and lock and extract dependencies, sorting
#' along the way using the combine_dependencies function.
#' @param master_list list.  Packages to go through and parse dependencies from
#' @param lock list.  Original list of locked packages
#' @param previously_parsed_deps list.  List of packages and their dependencies that
#'   we have already parsed out of their respective description files
package_list <- function(master_list, lock, previously_parsed_deps) {
  ## Start off with the master list as our set of packages
  current_dependencies <- master_list
  for (i in seq_along(master_list)) {
    package <- master_list[[i]]

    ## Create a new list for this package if it no dependencies have been parsed
    ## for any version of the package.
    if (!package$name %in% names(previously_parsed_deps)) previously_parsed_deps[[package$name]] <- list()
    if (paste(package$version) %in% names(previously_parsed_deps[[package$name]])) {
      single_package_dependencies <-
        previously_parsed_deps[[package$name]][[
          paste(package$version)]]$dependencies
      current_dependencies[[package$name]] <-
        previously_parsed_deps[[package$name]][[
          paste(package$version)]]$package
    } else {
      dependency_output <- get_dependencies(
       structure(package
         , class = c(package$remote %||% "CRAN"
           , class(package)))
       , lock)
      package <- dependency_output$package
      single_package_dependencies <- dependency_output$dependencies

      ## Our package comes out of dependency search with a download_path
      ## attached.  We will use this for installation (assuming we keep this
      ## version of the package.
      current_dependencies[[package$name]] <- package

      ## Store the dependencies and corresponding package object in our humongous
      ## previously_parsed_deps object using version as the second key level
      previously_parsed_deps[[package$name]][[paste(package$version)]] <-
        list(package = package, dependencies = single_package_dependencies)
    }
    ## Now combine the dependencies from this package with our big dependency
    ## list.

    current_dependencies <- combine_dependencies(
      single_package_dependencies
      , current_dependencies
      , package$name)
  }

  ## If we have not altered the list, then we are done.  Otherwise, we run
  ## through the entire list again, because we have a bunch of new packages
  if (identical(master_list, current_dependencies)) return(master_list)
  else Recall(current_dependencies, lock, previously_parsed_deps)
}

## Attach the latest available lockbox version to a package
add_latest_version_in_lockbox <- function(package) {
  package$latest_version_in_lockbox <- max_package_version(
    list.files(file.path(lockbox_library(), package)))
  package
}

## Take a vector of versions and find the max without coercing to package_version
max_package_version <- function(versions) {
  # Only consider versions that are parsable as versions
  versions <- versions[!is.na(package_version(versions, strict = FALSE))]
  if (length(versions) == 0) return(NULL)
  formatted_versions <- package_version(as.character(versions))
  versions[which(formatted_versions == max(formatted_versions))[1]]
}

## Check a dependency list for inclusion in the lockfile and replace the package
## with the locked version if it does appear there.  Also throw
## an error if we require a dependency version greater than that specified by
## the lockfile.
replace_with_lock <- function(package, lock) {
  lock_names <- vapply(lock, `[[`, character(1), "name")
  if (package$name %in% lock_names) {
    locked_package <- Find(function(l) l$name == package$name, lock)
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
  if (package$is_dependency_package) {
      package$latest_version <- package$latest_version_in_lockbox %||%
        package$latest_version %||% get_latest_version(package)
      if(is.null(package$latest_version)) package <- NULL
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

## Combine two lists of dependencies via version comparisons.  Keep packages
## found in list1 on the left side of the entirety of list2, while moving
## the parent package to the space after it's rightmost dependency found in list2.
combine_dependencies <- function(list1, list2, current_parent) {
  if (length(list1) == 0) return(list2)
  names <- lapply(list(list1, list2), function(lst) vapply(lst, `[[`, character(1), "name"))
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

## Swap packages by comparing version information. 1ist2 has already
## been ordered by dependency, so is imperative that we keep its order while
## potentially swapping in the corresponding packages in list1 that have later
## version requirements.
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

  ## Swap a package from list1 into list2
  list2_swap <- vapply(names1[swap_package2for1]
    , function(n) which(names2 == n)
    , integer(1))

  list2[list2_swap] <- list1[swap_package2for1]
  list2
}

## Either use the current lockbox library DESCRIPTION
## file or download the accurate remote DESCRIPTION file.
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
        , dirname(lockbox_package_path(locked_package))))
  } else {
    cat(crayon_blue("."))
    output <- tryCatch(get_remote_dependencies(package), error = function(e) e)
    if (methods::is(output, "error")) {
      stop(crayon_red(paste0("Dependencies could not be resolved for package: "
        , package$name, " version: ", package$version
        , " remote: ", package$remote, " due to error: ", output)))
    } else {
      package <- output$package
      dependencies <- output$dependencies
    }
  }
  dependencies <- strip_dependencies(dependencies, package, lock)
  list(package = package, dependencies = dependencies)
}

strip_dependencies <- function(dependencies, package, lock) {
  dependencies <- strip_duplicate_dependencies(dependencies)
  dependencies <- strip_pesky_dependencies(dependencies)
  dependencies <- strip_core_dependencies(dependencies)
  dependencies <- lapply(dependencies, add_latest_version_in_lockbox)
  dependencies <- lapply(dependencies, function(dep) {
    dep$parent_package <- package$name
    dep})
  dependencies <- lapply(dependencies, replace_with_lock, lock)
  Filter(dependencies, f = Negate(is.null))
}

## Remove pesky_namespace dependencies
strip_pesky_dependencies <- function(dependencies) {
  dependencies[!vapply(dependencies, `[[`, character(1), "name") %in% pesky_namespaces]
}

## Certain packages are no longer on cran but incorporated into R Core
strip_core_dependencies <- function(dependencies) {
  core_pkgs <- as.character(installed.packages(priority = "base")[,1])
  dependencies[!vapply(dependencies, `[[`, character(1), "name") %in% core_pkgs]
}

## We don't trust package authors to only put a package in once
strip_duplicate_dependencies <- function(dependencies) {
  dependencies[!duplicated(vapply(dependencies, `[[`, character(1), "name"))]
}

## Get the dependencies for a given package
get_remote_dependencies <- function(package) {
  UseMethod("get_remote_dependencies")
}

## If a package is local we just read from the directory given
get_remote_dependencies.local <- function(package) {
  description_name <- file.path(package$dir, "DESCRIPTION")
  list(package = package
    , dependencies =
      dependencies_from_description(package, read.dcf(description_name)))
}

## For packages on CRAN we will extract to a temporary directory when we
## download the accurate remote DESCRIPTION file. Because these are tarballs
## there is no simple way to extract only our desired file like we can with
## zipfiles using the unz function.
get_remote_dependencies.CRAN <- function(package) {
  original_version <- package$version
  if (package$is_dependency_package) {
    package$version <- NA
  }
  filepath <- download_package(package)

  sep <- .Platform$file.sep
  split_fp <- strsplit(filepath,sep)[[1]]
  dirpath <- dirname(filepath)
  file_list <- tryCatch(untar(filepath, list = TRUE))
  if (length(file_list) == 0 || identical(attr(file_list, "status"), 1L)) {
    filepath <- download_package(package, force = TRUE)
    file_list <- untar(filepath, list = TRUE)
  }
  description_name <- file_list[grepl(paste0("^[^", sep, "]+", sep
    ,"DESCRIPTION$"), file_list)]
  untar(filepath, description_name, exdir = dirpath)
  description_path <- file.path(dirpath, description_name)
  package$download_path <- filepath
  package$version <- original_version
  dcf <- read.dcf(file = description_path)
  if (package$is_dependency_package) {
    package$latest_version <- version_from_description(package$name, dcf)
  }
  list(package = package, dependencies = dependencies_from_description(package, dcf))
}

## Download the accurate remote DESCRIPTION file for a github repo.
get_remote_dependencies.github <- function(package) {
  output <- download_description_github(package)
  list(package = output$package
    , dependencies = dependencies_from_description(package, output$dcf))
}

download_description_github <- function(package) {
  remote <- package$remote
  filepath <- download_package(structure(
    package,
    class = c(remote, class(package))))
  package$download_path <- filepath
  file_list <- try(unzip(filepath, list = TRUE))
  if (methods::is(file_list, "try-error")) {
    filepath <- download_package(structure(
      package,
      class = c(remote, class(package))), force = TRUE)
    file_list <- unzip(filepath, list = TRUE)
  }
  sep <- .Platform$file.sep
  subdir <- ""
  if (!is.null(package$subdir)){
    subdir <- paste0(sep, package$subdir)
  }
  description_name <- file_list$Name[grepl(paste0("^[^", sep, "]+"
    , subdir, sep, "DESCRIPTION$"), file_list$Name)]
  file_con <- unz(filepath, description_name)
  dcf <- read.dcf(file = file_con)
  close(file_con)
  list(package = package, dcf = dcf)
}

version_from_remote <- function(package) {
  output <- download_description_github(package)
  version_from_description(package
    , output$dcf)
}

version_from_description <- function(package_name, dcf) {
  if (is.element("Version", colnames(dcf))) {
    as.character(dcf[1, "Version"])
  } else {
    NA_character_
  }
}

## Parse dependencies from description
dependencies_from_description <- function(package, dcf) {
  ## We install 4 kinds of dependencies listed in the description file. If our
  ## dcf does not contain any of these elements we have no dependencies to
  ## speak of
  dependency_levels <- c("Depends", "Imports", "LinkingTo", "Remotes")
  if (!any(dependency_levels %in% colnames(dcf))) return(list())

  ## The parse_dcf function returns a matrix where rows correspond to packages
  ## and column 1 corresponds to the package name and column 3 corresponds to
  ## its version requirement.  The rownames of this matrix are the type of 
  ## dependency (Depends, Imports, LinkingTo, or Remotes)
  dependencies_parsed <- as.data.frame(parse_dcf(dcf
    , depLevel = dependency_levels[dependency_levels %in%
      colnames(dcf)])[[package$name]])
  
  if(NROW(dependencies_parsed) == 1 && NCOL(dependencies_parsed) == 1) return(list())

  ## We separate out non-remote dependencies from remote dependencies, because
  ## they require different logic
  non_remote_list <- get_non_remote_list(dependencies_parsed)
  remote_list <- get_remote_list(dependencies_parsed)

  ## Remote package names are duplicated in Depends, LinkingTo, and Imports entries
  non_remote_list <- non_remote_list[!is.element(
    vapply(non_remote_list, `[[`, character(1), "name")
    , vapply(remote_list, `[[`, character(1), "name"))]

  c(non_remote_list, remote_list)
}

get_non_remote_list <- function(dependencies_parsed) {
  non_remote_dependencies <- dependencies_parsed[!grepl("^Remotes", rownames(dependencies_parsed)), ]

  ## Remove the Depends entry that just corresponds to the R version requirements
  non_remote_dependencies <- non_remote_dependencies[!grepl("^[rR]$"
    , non_remote_dependencies[,1]), , drop = FALSE]

  ## Parse the non-remote dependencies into a list of packages
  if (identical(NROW(non_remote_dependencies),0L)){
    list()
  } else {
    Map(function(u,v) list(name = u, version = v)
      , as.character(non_remote_dependencies[,1]), as.character(non_remote_dependencies[,3]))
  }
}

get_remote_list <- function(dependencies_parsed) {
  remote_dependencies <- dependencies_parsed[grepl("^Remotes", rownames(dependencies_parsed)), ]

  ## Parse the remote dependencies into a list of packages
  if (identical(NROW(remote_dependencies),0L)){
    list()
  } else {
    matches_github <- grepl("github::", remote_dependencies[,1], fixed = TRUE)

    ## We do not currently support non-github remotes
    matches_unsupported <- grepl("bitbucket::|svn::|url::|local::|gitorious"
      , remote_dependencies[,1])
    if (any(matches_unsupported)) {
      stop(paste0("Package ", package$name, " from repo ", package$repo
        , " has unsupported (non-github) remote dependencies"))
    } else {
      Map(extract_package_from_remote
        , as.character(remote_dependencies[,1])
        , as.character(remote_dependencies[,3])
        , matches_github)
    }
  }
}

extract_package_from_remote <- function(original_name, version, matches_github) {
  name <- original_name

  ## if github is explicitly stated as the remote then we remove
  ## such references
  if (matches_github){
    name <- gsub("git::.*github\\.com/", "", name)
    name <- gsub("\\.git", "", name)
  }

  ## We extract the package name and repo name from the entry
  ## Could potentially fail if the repo is named something different 
  ## than its package name.  Other option is to download it now
  ## and parse it on the fly, but if we do that we have to do it
  ## every time we load, since we don't know its package name to
  ## look it up in our lockbox directory
  subname <- gsub("^.*/", "", original_name)
  subname <- gsub("@.*", "", subname)
  subrepo <- gsub("@.*", "", original_name)
  pkg <- list(name = subname
    , repo = subrepo
    , version = version
    , remote = "github")
  if (grepl("@", name)) {
    pkg$ref <- gsub(".*@", "", name)
  }
  pkg
}
