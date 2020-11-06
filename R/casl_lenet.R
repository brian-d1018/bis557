#' @title Linear Elastic Net
#' @description Compute linear elastic net using coordinate descent. Need
#' `alpha = 1` for LASSO Regression. This function is from the BIS 557 Textbook
#' (Chapter 7, Page 191).
#'
#' @param X A numeric data matrix
#' @param y Response vector
#' @param lambda The penalty term
#' @param alpha Value from 0 and 1; balance between l1/l2 penalty
#' @param b Initialize regression coefficients
#' @param tol Numeric tolerance parameter
#' @param maxit Integer maximum number of iterations
#' @param W Vector of sample weights
#'
#' @return Regression vector beta of length ncol(X)
#'
#' @export

casl_lenet <- function(X, y, lambda, alpha = 1,
                       b = matrix(0, nrow = ncol(X), ncol = 1), tol = 1e-5,
                       maxit = 50L, W = rep(1, length(y))/length(y)) {
  for (j in seq_along(lambda)) {
    if (j > 1) {
      b[,j] <- b[, j-1, drop = FALSE]
    }

    # Update the slope coefficients until they converge.
    for (i in seq(1, maxit)) {
      b_old <- b[,j]
      b[,j] <- casl_lenet_update_beta(X, y, lambda[j], alpha, b[,j], W)
      if (all(abs(b[,j] - b_old) < tol)) break
    }
    if (i == maxit) warning("Function lenet did not converge.")
  }
  return(b)
}
