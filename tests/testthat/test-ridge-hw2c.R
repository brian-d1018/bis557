library(testthat)

context("Test the output of homework 2c - ridge regression.")

test_that("Your ridge_hw2c() function is the same as lm() when lambda_val = 0.", {

  data(iris)

  b_ridge <- ridge_hw2c(Sepal.Length ~ ., iris, lambda_val = 0)

  fit_lm <- lm(Sepal.Length  ~ ., iris)

  expect_equivalent(fit_lm$coefficients, b_ridge$coefficients,
                    tolerance = 1e-12)
})

test_that("Your ridge_hw2c() function works with contrasts.", {

  data(iris)

  b_ridge <- ridge_hw2c(Sepal.Length ~ ., iris, lambda_val = 1,
                        contrasts = list(Species = "contr.sum"))

  expect_equivalent(b_ridge$coefficients, c(1.04, 0.65, 0.84, -0.29, 0.57, -0.10),
                    tolerance = 1e-1)
})

test_that("Your ridge_hw2c() function works in a tougher case.", {

  data(lm_patho)

  b_ridge <- ridge_hw2c(y ~ ., lm_patho, lambda_val = 1e4)

  expect_equivalent(b_ridge$coefficients, c(2e-9, 1e0, -1e-9),
                    tolerance = 1e-5)
})
