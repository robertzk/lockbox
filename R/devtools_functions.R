# All functions in this file were shamelessly stolen from devtools to decrease
# the dependency requirements of lockbox.  They remain unaltered

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
#' Resolve the ref
github_resolve_ref <- function(x, params) UseMethod("github_resolve_ref")

github_resolve_ref.default <- function(x, params) {
  params$ref <- x
  params
}

github_resolve_ref.NULL <- function(x, params) {
  params$ref <- "master"
  params
}

github_resolve_ref.github_pull <- function(x, params) {
  # GET /repos/:user/:repo/pulls/:number
  path <- file.path("repos", params$username, params$repo, "pulls", x)
  response <- github_GET(path)

  params$username <- response$head$user$login
  params$ref <- response$head$ref
  params
}

github_resolve_ref.github_release <- function(x, params) {
  # GET /repos/:user/:repo/releases
  path <- paste("repos", params$username, params$repo, "releases", sep = "/")
  response <- github_GET(path)
  if (length(response) == 0L)
    stop("No releases found for repo ", params$username, "/", params$repo, ".")

  params$ref <- response[[1L]]$tag_name
  params
}
#' Get method for github
github_GET <- function(path, ..., pat = github_pat()) {
  auth <- github_auth(pat)
  req <- httr::GET("https://api.github.com/", path = path, auth, ...)
  github_response(req)
}

github_auth <- function(token) {
  if (is.null(token)) {
    NULL
  } else {
    httr::authenticate(token, "x-oauth-basic", "basic")
  }
}

github_response <- function(req) {
  text <- httr::content(req, as = "text")
  parsed <- jsonlite::fromJSON(text, simplifyVector = FALSE)

  if (httr::status_code(req) >= 400) {
    errors <- vapply(parsed$errors, `[[`, "message", FUN.VALUE = character(1))

    stop(
      parsed$message, " (", httr::status_code(req), ")\n",
      paste("* ", errors, collapse = "\n"),
      call. = FALSE
    )
  }

  parsed
}
