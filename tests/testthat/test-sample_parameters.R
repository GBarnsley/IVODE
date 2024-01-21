test_that("no sample", {
  out <- sample_parameters("Hib disease")
  expect_true(is.list(out))
  expect_equal(length(out), 3)
  expect_equal(names(out), c("duration_of_pre_infectious", "R0", "duration_of_infectious"))
})

test_that("no sample", {
  n_samples <- 10
  out <- sample_parameters("Hib disease", n_samples)
  expect_true(is.list(out))
  expect_equal(length(out), 3)
  expect_equal(length(out[[1]]), n_samples)
  expect_equal(length(out[[2]]), n_samples)
  expect_equal(length(out[[3]]), n_samples)
  expect_equal(names(out), c("duration_of_pre_infectious", "R0", "duration_of_infectious"))
})