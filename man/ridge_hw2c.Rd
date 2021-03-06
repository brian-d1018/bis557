% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ridge_hw2c.R
\name{ridge_hw2c}
\alias{ridge_hw2c}
\title{OLS with Ridge Regression}
\usage{
ridge_hw2c(form, d, lambda_val, contrasts = NULL)
}
\arguments{
\item{form}{linear model formula}

\item{d}{data frame}

\item{lambda_val}{the lambda value (regularization parameter)}

\item{contrasts}{optional list of constants for factor variables aka contrast}
}
\value{
the best-fit coefficients with ridge regularization
}
\description{
This function fits linear models with the ridge regression
regularization (using the l2 norm). This method is used to reduce overfitting.
}
\examples{
data(iris)
ridge_hw2c(Sepal.Length ~ ., iris, lambda_val = 1)

}
