#' @title OLS with Ridge Regression
#' @description This function fits linear models with the ridge regression
#' regularization (using the l2 norm). This method is used to reduce overfitting.
#'
#' @param form linear model formula
#' @param d data frame
#' @param lambda_val the lambda value (regularization parameter)
#' @param contrasts optional list of constants for factor variables aka contrast
#'
#' @examples
#' data(iris)
#' ridge_hw2c(Sepal.Length ~ ., iris, lambda_val = 1)
#'
#' @import stats
#' @export

ridge_hw2c <- function(form, d, lambda_val, contrasts=NULL) {
  # Find estimated OLS coefficients using SVD
  if (is.null(contrasts) == TRUE) {
    X <- model.matrix(form, d)
  } else {
    X <- model.matrix(form, d, contrasts.arg = contrasts)
  }
  y <- get_all_vars(form, d)[,1]

  # SVD
  Xsvd <- svd(X); U <- Xsvd$u; V <- Xsvd$v; sigmas <- Xsvd$d
  D <- diag(sigmas / (sigmas^2 + lambda_val))
  b_hat <- V %*% D %*% t(U) %*% y

  my_list <- list("coefficients" = b_hat)
  return(my_list)
}
