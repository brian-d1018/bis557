library(testthat)

context("Test the output of CASL linear elastic net function.")

test_that("The book's casl_lenet() function works!", {

  data(iris)

  X <- model.matrix( ~ . - Sepal.Length - Species, data = iris)

  y <- matrix(iris$Sepal.Length, ncol = 1)

  b_hat <- casl_lenet(X, y, lambda = 5, alpha = 0.2, maxit = 1e3L)

  expect_equivalent(b_hat, c(0.0433, 0.6617, 0.6846, 0.0259),
                    tolerance = 1e-1)
})
