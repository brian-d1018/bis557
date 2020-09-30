library(testthat)

context("Test the output of homework 2d - ridge regression and optimizing lambda.")

test_that("Your ridge_hw2d() function works for Iris data.", {

  set.seed(2020)

  data(iris)

  lm_ridge <- ridge_hw2d(Sepal.Length ~ ., iris[,-5],
                           lambda_vals = 10^seq(-3, 2, length = 200))

  expect_equivalent(lm_ridge$min_lambda, 0.11,
                    tolerance = 5e-2)
})

test_that("Your ridge_hw2d() function optimizes for Iris data with contrasts.", {

  set.seed(2020)

  data(iris)

  lm_ridge <- ridge_hw2d(Sepal.Length ~ ., iris,
                         lambda_vals = 10^seq(-3, 2, length = 200),
                         contrasts = list(Species = "contr.sum"))

  expect_equivalent(lm_ridge$min_lambda, 4.40,
                    tolerance = 5e-2)
})

test_that("Your ridge_hw2d() function works with polynomials.", {

  set.seed(2020)

  x <- seq(-12, 12, by = 0.1)

  df <- data.frame("y" = 7 - 2*x - 1.5*x^2 + 0.5*x^3 + rnorm(length(x)),
                   "x1" = x, "x2" = x^2, "x3" = x^3)

  lm_ridge <- ridge_hw2d(form = y ~ ., d = df,
                         lambda_vals = 10^seq(-3, 2, length = 200))

  expect_equivalent(lm_ridge$min_lambda, 0.0114,
                    tolerance = 5e-2)
})
