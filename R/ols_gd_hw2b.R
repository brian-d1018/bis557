#' @title OLS with Gradient Descent
#' @description This function uses the gradient descent algorithm (matrix form)
#' to solve OLS and the penalty based on out-of-sample accuracy.
#'
#' @param form linear model formula
#' @param d data frame
#' @param b_0 parameters of model: column vector of initialized coefficients
#' @param learn_rate the initialized learning rate (aka step size)
#' @param max_iter the maximum number of iterations for this algorithm
#' @param contrasts optional list of constants for factor variables aka contrast
#'
#' @return the estimated coefficients, as well as the penalty (loss)
#'
#' @examples
#' set.seed(2020)
#' x <- -30:30
#' df <- data.frame(y = 6 + 7*x + rnorm(length(x)), x = x)
#' bis557::ols_gd_hw2b(form = y ~ ., d = df, b_0 = rep(1e-16, 2),
#'                     learn_rate = 0.18, max_iter = 1e5)
#'
#' @import stats
#' @export

ols_gd_hw2b <- function(form, d, b_0, learn_rate, max_iter, contrasts=NULL) {
  # build the model matrix X, without the intercept column at first
  if (is.null(contrasts) == TRUE) {
    X <- model.matrix(form, d)[,-1]
    y <- get_all_vars(form, d)[,1]
  } else {
    X <- model.matrix(form, d, contrasts.arg = contrasts)[,-1]
    y <- get_all_vars(form, d)[,1]
  }

  # Estimated y vs Actual y (Error L2 norm)
  b <- grad_descent(X, y, b_0, learn_rate, max_iter)
  y_hat <- model.matrix(form, d) %*% as.vector(b)
  errors <- sum( (y - y_hat)^2 )
  my_list <- list("coefficients" = b, "penalty_error" = errors)
  return(my_list)
}
