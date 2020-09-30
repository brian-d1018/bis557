#' @title OLS with Gradient Descent
#' @description This function uses the gradient descent algorithm (matrix form)
#' to solve OLS using 10-fold cross validation (to calculate out-of-sample
#' accuracy).
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
#' @import rsample
#' @export

ols_gd_hw2b <- function(form, d, b_0, learn_rate, max_iter, contrasts=NULL) {
  # build the model matrix X, without the intercept column at first
  if (is.null(contrasts) == TRUE) {
    folds <- vfold_cv(data = d, v = 10)
    errors <- numeric()
    betas <- matrix(0, nrow = length(b_0), ncol = length(folds$id))
    for (j in 1:length(folds$id)) {
      d_train <- analysis(folds$splits[[j]])
      d_test <- assessment(folds$splits[[j]])
      X_train <- model.matrix(form, d_train)[,-1]
      X_test <- model.matrix(form, d_test)[,-1]
      y_train <- get_all_vars(form, d_train)[,1]
      y_test <- get_all_vars(form, d_test)[,1]

      # Estimated y vs Actual y (Error L2 norm)
      b <- grad_descent(X_train, y_train, b_0, learn_rate, max_iter)
      y_test_hat <- model.matrix(form, d_test) %*% as.vector(b)
      errors[j] <- mean( (y_test - y_test_hat)^2 )
      betas[,j] <- b
    }
  } else {
    folds <- vfold_cv(data = d, v = 10)
    errors <- numeric()
    betas <- matrix(0, nrow = length(b_0), ncol = length(folds$id))
    for (j in 1:length(folds$id)) {
      d_train <- analysis(folds$splits[[j]])
      d_test <- assessment(folds$splits[[j]])
      X_train <- model.matrix(form, d_train, contrasts.arg = contrasts)[,-1]
      X_test <- model.matrix(form, d_test, contrasts.arg = contrasts)[,-1]
      y_train <- get_all_vars(form, d_train)[,1]
      y_test <- get_all_vars(form, d_test)[,1]

      # Estimated y vs Actual y (Error L2 norm)
      b <- grad_descent(X_train, y_train, b_0, learn_rate, max_iter)
      y_test_hat <- model.matrix(form, d_test) %*% as.vector(b)
      errors[j] <- mean( (y_test - y_test_hat)^2 )
      betas[,j] <- b
    }
  }

  # Find the fold with the lowest MSE
  idx_min <- which.min(errors)
  my_list <- list("coefficients" = as.vector(betas[,idx_min]),
                  "CV_penalty_MSE" = mean(errors))
  return(my_list)
}
