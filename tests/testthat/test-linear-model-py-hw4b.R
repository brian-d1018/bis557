library(testthat)

context("Test the output of homework 4b - out-of-core linear model with Python.")

test_that("Your linear_model_py_hw4b() function works with 10 batches of Iris data.", {

  data(iris)

  K <- 10; n <- nrow(iris); p <- ncol(iris)

  betas <- matrix(nrow = K, ncol = p-1)

  for (i in 1:K) {
    b_batch <- linear_model_py_hw4b(form = Sepal.Length ~ .,
                                    d = iris[ceiling((i-1)*n/K + 1):ceiling(i*n/K), -5])
    betas[i,] <- b_batch$coefficients
  }

  b_hat_py <- colMeans(betas)

  expect_equivalent(b_hat_py, c(1.619, 0.489, 0.752, -0.586),
                    tolerance = 1e-1)
})

test_that("Your linear_model_py_hw4b() function works with 25 batches of Iris data.", {

  data(iris)

  K <- 25; n <- nrow(iris); p <- ncol(iris)

  betas <- matrix(nrow = K, ncol = p-1)

  for (i in 1:K) {
    b_batch <- linear_model_py_hw4b(form = Sepal.Length ~ .,
                                    d = iris[ceiling((i-1)*n/K + 1):ceiling(i*n/K), -5])
    betas[i,] <- b_batch$coefficients
  }

  b_hat_py <- colMeans(betas)

  expect_equivalent(b_hat_py, c(2.039, 0.558, 0.536, -0.428),
                    tolerance = 1e-1)
})

test_that("Your linear_model_py_hw4b() function works with a million rows of data with 100 batches.", {

  set.seed(2020)

  K <- 1e2; n <- 1e6; p <- 3

  X <- data.frame("y" = rnorm(n, sd = 2),
                  "x1" = rnorm(n, sd = 2),
                  "x2" = rnorm(n, sd = 2))

  betas <- matrix(nrow = K, ncol = p)

  for (i in 1:K) {
    b_batch <- linear_model_py_hw4b(form = y ~ .,
                                    d = X[ceiling((i-1)*n/K + 1):ceiling(i*n/K),])
    betas[i,] <- b_batch$coefficients
  }

  b_hat_py <- colMeans(betas)

  expect_equivalent(b_hat_py, c(1.079e-3, -7.384e-4, 6.082e-4),
                    tolerance = 1e-1)
})
