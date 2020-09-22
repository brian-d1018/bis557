library(testthat)

context("Test the output of homework 2b - OLS and gradient descent error.")

test_that("Your ols_gd_hw2b() function works in an easy case.", {

  data(iris)

  b_gd <- ols_gd_hw2b(form = Sepal.Length ~ ., d = iris[,-5], b_0 = rep(1e-9,4),
                      learn_rate = 2, max_iter = 2e5)

  expect_equivalent(as.vector(b_gd$coefficients), c(1.67, 0.56, 1.27, -1.94),
                    tolerance = 5e-1)

  expect_equivalent(b_gd$penalty_error, 26.25, tolerance = 1e0)
})

test_that("Your ols_gd_hw2b() function works for simple linear regression.", {

  set.seed(2020)

  x2 <- -30:30

  df <- data.frame(y = 6 + 7*x2 + rnorm(length(x2)), x = x2)

  b_gd <- ols_gd_hw2b(form = y ~ ., d = df, b_0 = rep(1e-16, 2),
                      learn_rate = 0.18, max_iter = 1e5)

  expect_equivalent(round(b_gd$coefficients), c(6, 7),
                    tolerance = 5e-1)

  expect_equivalent(b_gd$penalty_error, 82.60, tolerance = 5e0)
})

test_that("Your ols_gd_hw2b() function works for a cubic polynomial.", {

  set.seed(2020)

  x3 <- -15:15

  df <- data.frame("y" = 7 - 2*x3 - 1.8*x3^2 + 0.2*x3^3 + rnorm(length(x3)),
                   "x1" = x3, "x2" = x3^2, "x3" = x3^3)

  b_gd <- ols_gd_hw2b(form = y ~ ., d = df, b_0 = rep(1e-16, 4),
                      learn_rate = 3.2e-5, max_iter = 1e5)

  expect_equivalent(as.vector(b_gd$coefficients), c(0, -1.1, -1.1, 0.2),
                    tolerance = 1e0)

  expect_equivalent(b_gd$penalty_error, 7920,
                    tolerance = 1e2)
})
