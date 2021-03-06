% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grad_descent.R
\name{grad_descent}
\alias{grad_descent}
\title{Gradient Descent}
\usage{
grad_descent(X, y, b_0, learn_rate, max_iter)
}
\arguments{
\item{X}{matrix of all the predictors (excludes the column of 1's)}

\item{y}{column vector of target (response) values}

\item{b_0}{parameters of model: column vector of initialized coefficients}

\item{learn_rate}{the initialized learning rate (aka step size)}

\item{max_iter}{the maximum number of iterations for this algorithm}
}
\value{
the estimated coefficients/parameters
}
\description{
This function uses the gradient descent algorithm (matrix form)
to solve the coefficients of simple linear regression with least squares
error. This is an optimization algorithm to minimize a function which
updates the parameters of the model. A very common statistical and ML
algorithm.
}
\examples{
data(iris)
print(b <- grad_descent(X = iris[,2:4], y = iris[,1], b_0 = rep(1e-16, 4),
                        learn_rate = 2, max_iter = 2e5))

}
