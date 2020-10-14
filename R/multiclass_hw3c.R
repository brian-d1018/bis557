#' @title Multi-Class Logistic Regression
#' @description This function classifies observations for multiple classes.
#' This algorithm will use the "one-vs-all" approach.
#'
#' @param X Numeric (continuous or categorical) data matrix
#' @param y Categorical Response/Target vector (should be a factor)
#' @param maxiter Maximum number of iterations
#' @param tol Tolerance
#'
#' @return The predicted class/category for each observation
#'
#' @examples
#' data(iris)
#' multiclass_hw3c(X = iris[,-5], y = iris$Species, maxiter = 60)
#'
#' @export

multiclass_hw3c <- function(X, y, maxiter=50, tol=1e-12) {
  X <- as.matrix(X)
  K <- length(unique(y)) # number of classes
  y <- as.factor(y)      # make sure `y` is a factor

  # Store important data for each class
  betas <- matrix(0, nrow = K, ncol = ncol(X)) # betas for each class
  probs <- matrix(0, nrow = K, ncol = nrow(X)) # probabilities of each class

  # For each training dataset
  for (k in 1:K) {
    # Classify `y` as binary: either belongs to class `k` or does not
    y_bin <- as.numeric(y == levels(y)[k])
    beta <- betas[k,]

    for (j in 1:maxiter) {
      b_old <- beta # keep the old beta coefficient for comparison later
      p <- 1 / (1 + exp(-X %*% beta))     # probabilities
      D <- as.numeric(p * (1 - p))        # variance of probabilities
      H <- t(X) %*% diag(D) %*% X         # Hessian matrix
      score <- t(X) %*% (y_bin - p)       # Gradient
      beta <- beta + (solve(H) %*% score) # update beta
      if (sum((beta - b_old)^2) < tol) break
    }
    # Keep the probabilities for each observation for class `k`
    probs[k,] <- p
    betas[k,] <- beta
  }

  # For each observation, find the class with the highest probability
  which_k <- apply(X = probs, MARGIN = 2, FUN = which.max)

  # Prediction of classes
  y_pred <- levels(y)[which_k]
  return(y_pred)
}
