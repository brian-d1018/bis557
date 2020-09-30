#' @title Ridge Regression: Finding Lambda
#' @description This function finds the lambda which gives the lowest loss.
#'
#' @param form linear model formula
#' @param d data frame
#' @param lambda_vals the lambda values (regularization parameter)
#' @param contrasts optional list of constants for factor variables aka contrast
#'
#' @examples
#' data(iris)
#' ridge_hw2d(form = Sepal.Length ~ ., d = iris[,-5],
#'            lambda_vals = 10^seq(-3, 2, length = 200))
#'
#' @import stats
#' @import rsample
#' @export

ridge_hw2d <- function(form, d, lambda_vals, contrasts=NULL) {
  # For each lambda value
  loss <- numeric(length = length(lambda_vals))

  # Contrasts or No Contrasts
  if (is.null(contrasts) == TRUE) {
    # Cross Validation separately for each lambda
    for (i in 1:length(lambda_vals)) {
      folds <- vfold_cv(data = d, v = 10)
      errors <- numeric()

      # Calculate out-of-sample error for each fold
      for (j in 1:length(folds$id)) {
        d_train <- analysis(folds$splits[[j]])
        d_test <- assessment(folds$splits[[j]])
        X_test <- model.matrix(form, d_test)
        y_test <- get_all_vars(form, d_test)[,1]

        # Estimated y vs Actual y (Error L2 norm)
        b_hat <- ridge_hw2c(form = form, d = d_train,
                            lambda_val = lambda_vals[i])$coefficients
        y_test_hat <- X_test %*% as.vector(b_hat)
        errors[j] <- mean( (y_test - y_test_hat)^2 )
      }
      # CV MSE after considering all the folds
      loss[i] <- mean(errors)
    }
  } else {
    # Cross Validation separately for each lambda
    for (i in 1:length(lambda_vals)) {
      folds <- vfold_cv(data = d, v = 10)
      errors <- numeric()

      # Calculate out-of-sample error for each fold
      for (j in 1:length(folds$id)) {
        d_train <- analysis(folds$splits[[j]])
        d_test <- assessment(folds$splits[[j]])
        X_test <- model.matrix(form, d_test, contrasts.arg = contrasts)
        y_test <- get_all_vars(form, d_test)[,1]

        # Estimated y vs Actual y (Error L2 norm)
        b_hat <- ridge_hw2c(form = form, d = d_train,
                            lambda_val = lambda_vals[i])$coefficients
        y_test_hat <- X_test %*% as.vector(b_hat)
        errors[j] <- mean( (y_test - y_test_hat)^2 )
      }
      # CV MSE after considering all the folds
      loss[i] <- mean(errors)
    }
  }
  min_loss <- min(loss)
  min_lambda <- lambda_vals[which.min(loss)]

  my_list <- list("min_loss" = min_loss, "min_lambda" = min_lambda)
  return(my_list)
}
