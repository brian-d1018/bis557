library(testthat)

context("Test the output of homework 1 - gradient descent algorithm.")

test_that("Your grad_descent() function works in an easy case.", {

  data(iris)

  gd_iris <- grad_descent(X = iris[,2:4], y = iris[,1], b_0 = rep(1e-16, 4),
                          learn_rate = 1e-1, max_iter = 1e5)

  expect_equivalent(as.vector(gd_iris),
                    c(0.34078, 1.05988, 0.56646, 0.07184),
                    tolerance = 2e-1)
})

test_that("Your grad_descent() function works for an easy linear regression.", {

  set.seed(2020)

  x <- -30:30

  gd_x <- grad_descent(X = x, y = 6 + 7*x + rnorm(length(x)),
                       b_0 = rep(1e-16, 2), learn_rate = 0.18, max_iter = 1e5)

  expect_equivalent(round(as.vector(gd_x)), c(6, 7),
                    tolerance = 5e-1)
})

test_that("Your grad_descent() function works for a cubic polynomial.", {

  set.seed(2020)

  x3 <- -15:15

  gd_x3 <- grad_descent(X = cbind(x3, x3^2, x3^3),
                        y = 7 - 2*x3 - 1.8*x3^2 + 0.2*x3^3 + rnorm(length(x3)),
                        b_0 = rep(1e-16, 4), learn_rate = 3.2e-5, max_iter = 1e5)

  expect_equivalent(as.vector(gd_x3), c(0, -1.1, -1.1, 0.2),
                    tolerance = 8e-1)
})

test_that("Your grad_descent() function works in a tougher case.", {

  data(lm_patho)

  gd_patho <- grad_descent(X = lm_patho[,-1], y = lm_patho[,1],
                           b_0 = rep(1e-16, ncol(lm_patho)), learn_rate = 1.3e-16,
                           max_iter = 1e5)

  expect_equivalent(as.vector(gd_patho), c(7.183e-7, 1.000, 2.054e-7),
                    tolerance = 1e-5)
})
