library(testthat)

context("Test the output of CASL beta update.")

test_that("The book's casl_lenet_update_beta() function works!", {

  X <- matrix(1:20, nrow = 10, ncol = 2)
  y <- -2:7
  W <- rep(1, length(y))/length(y)

  b_hat <- casl_lenet_update_beta(X, y, lambda = 1, alpha = 1, b = 1:2, W)

  expect_equivalent(b_hat, c(-4.25974, 1.78787),
                    tolerance = 1e-1)
})
