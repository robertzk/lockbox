context("parse_lock")

test_that("it can parse a lockfile", {
  lock <- list(packages = list(list(name = "test", version = "0.2.2", repo = "test/test"),
                               list(name = "test-two", version = "2.5.6", repo = "test/test-two")))
  parsed_lock <- lockbox:::parse_lock(lock)
  expect_equal(length(parsed_lock), length(lock$packages))
  for (i in seq_along(lock)) {
    for (name in names(lock$packages[[i]])) {
      expect_is(parsed_lock[[i]][[name]], "character")
      expect_equal(parsed_lock[[i]][[name]], lock$packages[[i]][[name]])
    }
  }
})

test_that("it interprets a two digit version number", {
  lock <- list(packages = list(list(name = "test", version = 0.2, repo = "test/test")))
  parsed_lock <- lockbox:::parse_lock(lock)
  expect_equal(parsed_lock[[1]]$version, "0.2")
})

test_that("it interprets a one digit version number", {
  lock <- list(packages = list(list(name = "test", version = 12, repo = "test/test")))
  parsed_lock <- lockbox:::parse_lock(lock)
  expect_equal(parsed_lock[[1]]$version, "12")
})

test_that("it interprets a four digit version number", {
  lock <- list(packages = list(list(name = "test", version = "1.2.3.4", repo = "test/test")))
  parsed_lock <- lockbox:::parse_lock(lock)
  expect_equal(parsed_lock[[1]]$version, "1.2.3.4")
})

test_that("it can check for invalid config", {
  expect_error(lockbox:::parse_lock(list()), "Invalid config")
})

test_that("it rejects inputs that are not list or character", {
  expect_error(lockbox:::parse_lock(42), "Invalid parameters")
})

test_that("it can parse a yaml file", {
  parsed_lock <- lockbox:::parse_lock("data/test-basic.yml")
  expect_equal(length(parsed_lock), 1)
  expect_equal(parsed_lock[[1]][["name"]], "test")
  expect_equal(parsed_lock[[1]][["version"]], "1.0.0")
})

test_that("it can parse a yaml file with multiple packages", {
  parsed_lock <- lockbox:::parse_lock("data/test-multiple.yml")
  expect_equal(length(parsed_lock), 2)
  expect_equal(parsed_lock[[1]][["name"]], "test")
  expect_equal(parsed_lock[[1]][["version"]], "1.0.0")
  expect_equal(parsed_lock[[2]][["name"]], "other_test")
  expect_equal(parsed_lock[[2]][["version"]], "2.0.0")
})

test_that("version numbers on yaml files are correct", {
  parsed_lock <- lockbox:::parse_lock("data/test-versions.yml")
  expect_equal(parsed_lock[[1]][["version"]], "1.0")
  expect_equal(parsed_lock[[2]][["version"]], "1.1")
  expect_equal(parsed_lock[[3]][["version"]], "1")
})

describe("it can read envs", {
  packages <- list(list(name = "test", version = "1.2", repo = "test/test"),
                   list(name = "other_test", version = "3.2", repo = "test/other_test"))
  lock <- list(packages = packages, dev = "test", prod = "other_test")
  describe("dev env", {
    parsed_lock <- lockbox:::parse_lock(lock, env = "dev")
    test_that("it ignores other_test", {
      expect_equal(length(parsed_lock), 1)
      expect_equal(parsed_lock[[1]]$name, "test")
    })
  })
  describe("dev env", {
    parsed_lock <- lockbox:::parse_lock(lock, env = "prod")
    test_that("it ignores test", {
      expect_equal(length(parsed_lock), 1)
      expect_equal(parsed_lock[[1]]$name, "other_test")
    })
  })
  test_that("version numbers are still character", {
    for (env in c("dev", "prod")) {
      parsed_lock <- lockbox:::parse_lock(lock, env = env)
      for (element in parsed_lock[[1]]) {
        expect_is(element, "character")
      }
    }
  })
})

describe("it can read envs from a yaml", {
  describe("dev env", {
    parsed_lock <- lockbox:::parse_lock("data/test-envs.yml", env = "dev")
    test_that("it ignores other_test", {
      expect_equal(length(parsed_lock), 2)
      expect_equal(parsed_lock[[1]]$name, "test")
    })

    test_that("it can handle versions with more than one digit", {
      expect_equal(parsed_lock[[2]]$version, "0.999") 
    })
  })
  describe("prod env", {
    parsed_lock <- lockbox:::parse_lock("data/test-envs.yml", env = "prod")
    test_that("it ignores test", {
      expect_equal(length(parsed_lock), 1)
      expect_equal(parsed_lock[[1]]$name, "other_test")
    })
  })
  test_that("version numbers are still character", {
    for (env in c("dev", "prod")) {
      parsed_lock <- lockbox:::parse_lock("data/test-envs.yml", env = env)
      for (element in parsed_lock[[1]]) {
        expect_is(element, "character")
      }
    }
  })
})

test_that("it loads the example from the README", {
  parsed_lock <- lockbox:::parse_lock("data/test-readme-example.yml")
  expect_equal(names(parsed_lock[[1]]), c("name", "version", "repo"))
  expect_is(parsed_lock[[1]]$version, "character")
  expect_equal(names(parsed_lock[[2]]), c("name", "version", "repo", "load"))
  expect_is(parsed_lock[[2]]$version, "character")
  expect_is(parsed_lock[[2]]$load, "logical")
  expect_equal(names(parsed_lock[[3]]), c("name", "version"))
  expect_is(parsed_lock[[3]]$version, "character")
  expect_equal(names(parsed_lock[[4]]), c("name", "version", "dir"))
  expect_equal(names(parsed_lock[[5]]), c("name", "version", "dir", "autoinstall"))
  expect_is(parsed_lock[[5]]$version, "character")
  expect_is(parsed_lock[[5]]$autoinstall, "logical")
  expect_equal(names(parsed_lock[[6]]), c("name", "version", "repo"))
})
