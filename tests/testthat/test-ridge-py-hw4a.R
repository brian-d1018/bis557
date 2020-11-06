library(testthat)

context("Test the output of homework 4a - ridge regression with Python.")

test_that("Your Python ridge_py_hw4a() function is the same as R for lambda_val = 1.", {

  data(iris)

  y <- matrix(iris$Sepal.Length, ncol = 1)

  X <- model.matrix(~ . - Sepal.Length - Species, data = iris)

  b_ridge_py <- ridge_py_hw4a(y, X, lambda_val = 1)

  b_ridge <- ridge_hw2c(Sepal.Length ~ . - Species, iris, lambda_val = 1)

  expect_equivalent(b_ridge_py$coefficients, b_ridge$coefficients,
                    tolerance = 1e-12)
})

test_that("Your Python ridge_py_hw4a() function is the same as R for contrasts.", {

  data(iris)

  y <- matrix(iris$Sepal.Length, ncol = 1)

  X <- model.matrix(~ . - Sepal.Length, data = iris,
                    contrasts.arg = list(Species = "contr.sum"))

  b_ridge_py <- ridge_py_hw4a(y, X, lambda_val = 0.5)

  b_ridge <- ridge_hw2c(Sepal.Length ~ ., iris, lambda_val = 0.5,
                        contrasts = list(Species = "contr.sum"))

  expect_equivalent(b_ridge_py$coefficients, b_ridge$coefficients,
                    tolerance = 1e-12)
})

test_that("Your Python ridge_py_hw4a() function is the same as R in a tougher case.", {

  data(lm_patho)

  y <- matrix(lm_patho$y, ncol = 1)

  X <- model.matrix(~ . - y, data = lm_patho)

  b_ridge_py <- ridge_py_hw4a(y, X, lambda_val = 1e4)

  b_ridge <- ridge_hw2c(y ~ ., lm_patho, lambda_val = 1e4)

  expect_equivalent(b_ridge_py$coefficients, b_ridge$coefficients,
                    tolerance = 1e-5)
})
