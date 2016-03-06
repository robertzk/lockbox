context('random-dependencies')

test_that('Our random selection of CRAN and syberia packages can install', {
  test_dir <- tempfile()
  dir.create(test_dir)
  test_output <- test_random_lockbox(test_dir, num_random_cran = 1, seed = 0)
  unlink(test_dir, TRUE, TRUE)
  expect_true(test_output$success)
})
