library(testthat)

context("Test the output of homework 1 - gradient descent algorithm.")

test_that("Your grad_descent() function works in an easy case.", {

  data(iris)

  gd_iris <- grad_descent(X = iris[,2:4], y = iris[,1], b_0 = rep(1e-16, 4),
                          learn_rate = 2, max_iter = 2e5)

  expect_equivalent(as.vector(gd_iris),
                    c(1.4152552, 0.8890568, 0.2456270, 0.6444158),
                    tolerance = 1e-5)
})

test_that("Your grad_descent() function works in a tougher case.", {

  data(lm_patho)

  gd_patho <- grad_descent(X = lm_patho[,-1], y = lm_patho[,1],
                           b_0 = rep(1e-16, ncol(lm_patho)), learn_rate = 1.3e-16,
                           max_iter = 1e5)

  expect_equivalent(as.vector(gd_patho), c(7.183e-7, 1.000, 2.054e-7),
                    tolerance = 1e-5)
})
