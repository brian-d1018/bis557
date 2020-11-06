library(testthat)

context("Test the output of homework 4c - LASSO regression with Python.")

test_that("Your Python lasso_py_hw4c() function is similar to R for lambda_val = 0.01.", {

  n <- 10; p <- 5

  y <- (1:n)/2; X <- matrix((1:(n*p))^2 - 8*(1:(n*p)) + 9, nrow = n, ncol = p)

  b_lasso_py <- lasso_py_hw4c(y, qr.Q(qr(X)), lambda_val = 1e-2)

  b_lasso <- casl_lenet(qr.Q(qr(X)), y, lambda = 1e-2, tol = 1e-12, maxit = 1e4L)

  expect_equivalent(as.vector(b_lasso_py$coefficients), as.vector(b_lasso),
                    tolerance = 1e-2)
})

test_that("Your Python lasso_py_hw4c() function is similar to R for contrasts.", {

  data(iris)

  y <- matrix(iris$Sepal.Length, ncol = 1)

  X <- model.matrix(~ . - Sepal.Length, data = iris,
                    contrasts.arg = list(Species = "contr.sum"))

  b_lasso_py <- lasso_py_hw4c(y, qr.Q(qr(X)), lambda_val = 1e-3)

  b_lasso <- casl_lenet(qr.Q(qr(X)), y, lambda = 1e-3, tol = 1e-12, maxit = 1e4L)

  expect_equivalent(as.vector(b_lasso_py$coefficients), as.vector(b_lasso),
                    tolerance = 1e-2)
})

test_that("Your Python lasso_py_hw4c() function is similar to R in a tougher case.", {

  data(lm_patho)

  y <- matrix(lm_patho$y, ncol = 1)

  X <- model.matrix(~ . - y, data = lm_patho)

  b_lasso_py <- lasso_py_hw4c(y, qr.Q(qr(X)), lambda_val = 3.53e8)

  b_lasso <- casl_lenet(qr.Q(qr(X)), y, lambda = 3.53e8, tol = 1e-12, maxit = 1e4L)

  expect_equivalent(as.vector(b_lasso_py$coefficients), as.vector(b_lasso),
                    tolerance = 1e2)
})
