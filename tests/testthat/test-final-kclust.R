library(testthat)

context("Test the output of final k-means clustering function. This test is valid,
        since the rng `set.seed(2020)` is incorporated in the function already.")

test_that("Your final_kclust() clusters the final dataset to 5 groups correctly.", {

  x <- final_clean()

  expect_equal(dim(final_kclust(x, k = 5)$x_zip), c(30, 15))

  expect_equal(as.vector(final_kclust(x, k = 5)$x_cuts[1]), 1)
})

test_that("Your final_clean() clusters the final dataset to 7 groups correctly.", {

  x <- final_clean()

  expect_equal(dim(final_kclust(x, k = 7)$x_zip), c(30, 15))

  expect_equal(as.vector(final_kclust(x, k = 7)$x_cuts[8]), 4)
})
