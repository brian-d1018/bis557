#' @title OLS with Ridge Regression - Python
#' @description This function fits linear models with the ridge regression
#' regularization (using the l2 norm). This method is used to reduce overfitting.
#' Language is in Python 3.
#'
#' @param y target/response variable matrix
#' @param X independent variables matrix (contrasts should be taken care of here)
#' @param lambda_val the lambda value (regularization parameter)
#'
#' @return the best-fit coefficients with ridge regularization
#'
#' @examples
#' data(iris)
#' y <- matrix(iris$Sepal.Length, ncol = 1)
#' X <- model.matrix( ~ . - Sepal.Length - Species, data = iris)
#' ridge_py_hw4a(y, X, lambda_val = 1)
#'
#' @import stats
#' @import reticulate
#' @export

ridge_py_hw4a <- function(y, X, lambda_val) {
  # Import Python's Numpy
  np <- import("numpy", as = "np", convert = FALSE)
  # SVD
  svds <- r_to_py(np$linalg$svd(X, full_matrices = FALSE))
  # V needs to be transposed (difference between numpy and R)
  U <- svds[[0]]; sigs <- svds[[1]]; V <- svds[[2]]$T
  # Edit the diagonal `D` matrix to account for ridge regression
  D <- np$diag(np$divide(sigs, np$add(np$power(sigs, 2), lambda_val)))
  # Coefficients
  b_hat <- py_to_r(V$dot(D)$dot(U$T)$dot(y))

  my_list <- list("coefficients" = b_hat)
  return(my_list)
}
