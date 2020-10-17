#' @title GLM Using First-Order Gradient Descent
#' @description This function solves generalized linear models using only
#' first-order conditions, avoiding the Hessian matrix.
#' This uses the gradient descent algorithm, with either a constant or adaptive
#' step size.
#'
#' @param X Numeric (continuous or categorical) data matrix
#' @param y Response/Target vector
#' @param family Error distribution and link function, such as poisson or Gamma
#' @param steps Step size for each iteration
#' @param maxiter Maximum number of iterations
#' @param tol Tolerance
#'
#' @return Estimated beta coefficients
#'
#' @examples
#' set.seed(2020)
#' n <- 3000; p <- 4; maxiter <- 500; steps <- rep(1e-3, maxiter)
#' X <- cbind(1, matrix(rnorm(n * (p-1)), ncol = p-1))
#' beta <- c(-1, 0.2, 0.1, 0.3)
#' y <- rpois(n, lambda = exp(X %*% beta))
#' glm_hw3b(X, y, family = poisson(link = "log"), steps, maxiter)
#'
#' set.seed(2020)
#' steps2 <- 5e-3/(1:maxiter)
#' glm_hw3b(X, y, family = poisson(link = "log"), steps = steps2, maxiter)
#'
#' @import stats
#' @export

glm_hw3b <- function(X, y, family, steps, maxiter=1e3, tol=1e-12) {
  stopifnot(length(steps) == maxiter)
  beta <- rep(0, ncol(X))

  # Gradient Descent
  for (j in 1:maxiter) {
    b_old <- beta # keep the old beta coefficient for comparison later
    mu <- family$linkinv(X %*% beta) # E[y]: inverse link function
    score <- t(X) %*% (y - mu)       # Gradient (first-order)
    beta <- beta + steps[j] * score  # update beta
    if (sum((beta - b_old)^2) < tol) break
  }

  my_list <- list("coefficients" = beta)
  return(my_list)
}
