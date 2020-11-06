#' @title Update beta vector using coordinate descent
#' @description Update beta vector using coordinate descent. Need `alpha = 1`
#' for LASSO Regression. This function is from the BIS 557 Textbook (Chapter 7,
#' Page 190).
#'
#' @param X A numeric data matrix
#' @param y Response vector
#' @param lambda The penalty term
#' @param alpha Value from 0 and 1; balance between l1/l2 penalty
#' @param b A vector of initialized coefficients
#' @param W A vector of sample weights
#'
#' @return Regression vector with length ncol(X).
#'
#' @export

casl_lenet_update_beta <- function(X, y, lambda, alpha, b, W) {
  WX <- W * X
  WX2 <- W * X^2
  Xb <- X %*% b

  for (i in seq_along(b)) {
    Xb <- Xb - X[,i] * b[i]
    b[i] <- casl_util_soft_thresh(sum(WX[,i, drop=FALSE] * (y - Xb)),
                                  lambda*alpha)
    b[i] <- b[i] / (sum(WX2[,i]) + lambda * (1 - alpha))
    Xb <- Xb + X[,i] * b[i]
  }
  return(b)
}
