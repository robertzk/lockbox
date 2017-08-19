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
