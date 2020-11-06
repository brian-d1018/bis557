library(testthat)

context("Test the output of CASL soft threshold function.")

test_that("The book's casl_util_soft_thresh() function works!", {

  x <- casl_util_soft_thresh(a = -3:3, b = 1.5)

  expect_equivalent(x, c(-1.5, -0.5, 0, 0, 0, 0.5, 1.5),
                    tolerance = 1e-12)
})
