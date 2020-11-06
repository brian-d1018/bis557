#' @title OLS with LASSO Regression - Python
#' @description This function fits linear models with the LASSO regression
#' regularization (using the l1 norm). This method is used for subset selection.
#' Language is in Python 3.
#'
#' @param y target/response variable matrix
#' @param X independent variables matrix (contrasts should be taken care of
#' here) - orthonormal matrix recommended
#' @param lambda_val the lambda value (regularization parameter)
#'
#' @return the best-fit coefficients with LASSO regularization
#'
#' @examples
#' data(iris)
#' y <- matrix(iris$Sepal.Length, ncol = 1)
#' X <- model.matrix( ~ . - Sepal.Length - Species, data = iris)
#' lasso_py_hw4c(y, qr.Q(qr(X)), lambda_val = 1e-1)
#'
#' @import stats
#' @import reticulate
#' @export

lasso_py_hw4c <- function(y, X, lambda_val) {
  # Import Python's Numpy
  np <- import("numpy", as = "np", convert = FALSE)
  n <- nrow(X); p <- ncol(X)
  y <- r_to_py(y); X <- r_to_py(X)

  # Orthonormalize X
  Q <- np$linalg$qr(X)[0]

  # LASSO closed-form solution for orthonormal matrix
  maxfn <- np$maximum(np$subtract(np$abs(Q$T$dot(y)),
                                  np$multiply(n, lambda_val)), 0)
  b_hat <- py_to_r(np$multiply(np$sign(Q$T$dot(y)), maxfn))

  my_list <- list("coefficients" = b_hat)
  return(my_list)
}
