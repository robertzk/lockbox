context('random-dependencies')

test_that('Our random selection of CRAN and syberia packages can install', {
  test_dir <- tempfile()
  dir.create(test_dir)
  is_success <- tryCatch(
    test_random_lockbox(test_dir, num_random_cran = 1, seed = 0)$success
    , error = function(e) e)
  print(is_success)
  unlink(test_dir, TRUE, TRUE)
  expect_true(is_success)
})
