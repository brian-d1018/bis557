library(testthat)

context("Test the output of homework 2b - OLS and gradient descent error.")

test_that("Your ols_gd_hw2b() function works in an easy case.", {

  data(iris)

  set.seed(2020)

  b_gd <- ols_gd_hw2b(form = Sepal.Length ~ ., d = iris[,-5], b_0 = rep(1e-9,4),
                      learn_rate = 1, max_iter = 2e4)

  expect_equivalent(as.vector(b_gd$coefficients), c(0.54, 1.04, 0.63, -0.21),
                    tolerance = 5e-1)

  expect_equivalent(b_gd$CV_penalty_MSE, 0.13, tolerance = 2e-1)
})

test_that("Your ols_gd_hw2b() function works with contrasts.", {

  data(iris)

  set.seed(2020)

  b_gd <- ols_gd_hw2b(form = Sepal.Length ~ ., d = iris, b_0 = rep(1e-9,6),
                      learn_rate = 1, max_iter = 2e4,
                      contrasts = list(Species = "contr.sum"))

  expect_equivalent(as.vector(b_gd$coefficients),
                    c(0.45, 0.90, 0.72, -0.07, 0.39, 0.01),
                    tolerance = 5e-1)

  expect_equivalent(b_gd$CV_penalty_MSE, 0.27, tolerance = 2e-1)
})

test_that("Your ols_gd_hw2b() function works for simple linear regression.", {

  set.seed(2020)

  x2 <- -30:30

  df <- data.frame(y = 6 + 7*x2 + rnorm(length(x2)), x = x2)

  b_gd <- ols_gd_hw2b(form = y ~ ., d = df, b_0 = rep(1e-16, 2),
                      learn_rate = 0.2, max_iter = 2e4)

  expect_equivalent(round(b_gd$coefficients), c(6, 7),
                    tolerance = 5e-1)

  expect_equivalent(b_gd$CV_penalty_MSE, 364.77, tolerance = 2e1)
})

test_that("Your ols_gd_hw2b() function works for a cubic polynomial.", {

  set.seed(2020)

  x3 <- seq(-10, 10, by = 0.1)

  df <- data.frame("y" = 7 - 2*x3 - 1.5*x3^2 + 0.5*x3^3 + rnorm(length(x3)),
                   "x1" = x3, "x2" = x3^2, "x3" = x3^3)

  b_gd <- ols_gd_hw2b(form = y ~ ., d = df, b_0 = c(5, 1e-8, 1e-8, 1e-8),
                      learn_rate = 3e-4, max_iter = 2e4)

  expect_equivalent(as.vector(b_gd$coefficients), c(5.0, -0.0, -1.5, 0.5),
                    tolerance = 1e0)

  expect_equivalent(b_gd$CV_penalty_MSE, 25.29,
                    tolerance = 2e0)
})
