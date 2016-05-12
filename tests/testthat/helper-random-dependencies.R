get_hardcoded_github_yml <- function() return("packages:\n- name: stagerunner\n  version: 0.5.2\n  repo: syberia/stagerunner\n- name: objectdiff\n  version: 0.2.3.9000\n  repo: robertzk/objectdiff\n  ref: ab641a58523e7a1d78f82491838c70d5334d9603\n- name: tundra\n  version: 0.2.3\n  repo: syberia/tundra\n- name: director\n  version: 0.2.1\n  repo: syberia/director\n- name: mungebits\n  version: 0.3.13\n  repo: robertzk/mungebits\n- name: mungebits2\n  version: 0.1.0\n  repo: syberia/mungebits2\n  load: no\n- name: syberiaStages\n  version: 0.2.3\n  repo: robertzk/syberiaStages\n- name: statsUtils\n  version: 0.1.4\n  repo: robertzk/statsUtils\n- name: syberiaStructure\n  version: 0.2.2\n  repo: robertzk/syberiaStructure\n- name: devtools\n  repo: hadley/devtools\n  version: 1.10.0.9000\n  ref: 2b42b846534ceec47a867ad0376a7024ff80eb01\ndevelopment: ~\ntest: ~\nnothing: ~\n")

get_hardcoded_cran_yml <- function() return("packages:\n- name: Rcpp\n  version: 0.12.3\n- name: foreign\n  version: 0.8-66\n  load: no\n- name: crayon\n  version: 1.2.1\n  load: no\n- name: lubridate\n  version: 1.5.0\n- name: knitr\n  version: 1.12.3\n- name: memoise\n  version: 1.0.0\n")

test_lockbox <- function(test_dir) {
  github_packages <- yaml::yaml.load(get_hardcoded_github_yml())$packages
  cran_packages <- yaml::yaml.load(get_hardcoded_cran_yml())$packages
  install_dir <- tempfile(tmpdir = test_dir)
  dir.create(install_dir)
  result <- get_installation_result(github_packages, cran_packages, install_dir)
  unlink(install_dir, TRUE, TRUE)
  result
}

get_installation_result <- function(github_packages, cran_packages, install_dir
  , logfile_name = "logfile", final_msg = "Successfully loaded lockbox") {
    write_yml(cran_packages, github_packages, install_dir)

    lockbox_dir <- file.path(install_dir, "lockbox")
    dir.create(lockbox_dir)

    r_exec <- get_rscript_command(lockbox_dir, logfile_name, final_msg, install_dir)
    system(r_exec)

    unlink(lockbox_dir, TRUE, TRUE)
    unlink(paste0(lockbox_dir,"_transient"), TRUE, TRUE)
    unlink(paste0(lockbox_dir,"_transient_staging"), TRUE, TRUE)
    output <- suppressWarnings(readLines(file.path(install_dir, logfile_name)))
    list(success = grepl(final_msg, tail(output, 1)), log = output)
}

write_yml <- function(cran_packages, github_packages, install_dir) {
  all_packages <- c(github_packages, cran_packages)
  all_packages <- all_packages[sample(seq_along(all_packages))]
  lockfile_yml <- yaml::as.yaml(list(packages = all_packages
    , development = NULL, test = NULL, nothing = NULL))
  lockfile_path <- file.path(install_dir, "generated_lockfile.yml")
  if (file.exists(lockfile_path)) unlink(lockfile_path, TRUE, TRUE)
  fileConn <- file(lockfile_path)
  writeLines(lockfile_yml, fileConn)
  close(fileConn)
}

get_rscript_command <- function(lockbox_dir, logfile_name, final_msg, install_dir) {
  option_cmd1 <- paste0("options(lockbox.directory = '", lockbox_dir,"');")
  option_cmd2 <- paste0("options(lockbox.transient_dir = '", lockbox_dir
    , "_transient","');")
  lock_cmd <- paste0("setwd('", dirname(lockbox_dir),"');library(methods);lockbox::lockbox('generated_lockfile.yml');")
  option_cmd_sink <- paste0("sink('", logfile_name,"', split = TRUE);")
  msg_cmd <- paste0("cat('", final_msg, "')")
  paste0("cd ", install_dir, ";Rscript -e ","\"", option_cmd1, option_cmd2
    , lock_cmd, option_cmd_sink, msg_cmd, "\"")
}

## Not currently in use, but left in case we want to do randomized tests in the future
get_random_cran_packages <- function(num, seed = NULL, repo = "http://cran.r-project.org") {
  available <- available.packages(contriburl =
    contrib.url(repos = repo, type = "binary"))
  available <- data.frame(unique(available[, c("Package", "Version")]))
  set.seed(seed)
  available <- available[sample(NROW(available), num), , drop = FALSE]
  packages <- Map(
    function(p,v) list(name = as.character(p), version = as.character(v), load = FALSE)
    , available$Package, available$Version)
  names(packages) <- NULL
  packages
}
