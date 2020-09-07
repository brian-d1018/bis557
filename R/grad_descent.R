#' @title Gradient Descent
#' @description This function uses the gradient descent algorithm (matrix form)
#' to solve the coefficients of simple linear regression with least squares
#' error. This is an optimization algorithm to minimize a function which
#' updates the parameters of the model. A very common statistical and ML
#' algorithm.
#'
#' @param X matrix of all the predictors (excludes the column of 1's)
#' @param y column vector of target (response) values
#' @param b_0 parameters of model: column vector of initialized coefficients
#' @param learn_rate the initialized learning rate (aka step size)
#' @param max_iter the maximum number of iterations for this algorithm
#'
#' @return the estimated coefficients/parameters
#'
#' @examples
#' data(iris)
#' grad_descent(X = iris[,2:4], y = iris[,1], b_0 = rep(1e-16, 4),
#'              learn_rate = 2, max_iter = 2e5)
#'
#' @export

grad_descent <- function(X, y, b_0, learn_rate, max_iter) {
  # number of rows and columns
  m <- length(y)
  n <- length(b_0)

  # add the "1" column for matrix X
  X <- cbind(rep(1, m), as.matrix(X))
  y <- as.matrix(y)

  # initialized betas -> coefficients
  b <- as.matrix(b_0)

  # tolerance and convergence
  tol <- 1e-8

  for (i in 1:max_iter) {
    # define the loss function (mean squared error)
    loss <- (1 / (2*m)) * sum( (y - X %*% b)^2 )

    # gradient of loss function w.r.t. beta
    delta_loss <- -(1 / m) * (t(X) %*% (y - X %*% b) )

    # gradient descent -> improve the coefficient
    b <- b - (learn_rate / i) * delta_loss

    # new loss / "cost"
    new_loss <- (1 / (2*m)) * sum( (y - X %*% b)^2 )

    # stop algorithm if it "converges"
    if (abs(new_loss - loss) < tol) {
      break
    }
  }
  return(b)
}
