% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear_model_py_hw4b.R
\name{linear_model_py_hw4b}
\alias{linear_model_py_hw4b}
\title{Fitting Linear Model Function - Python}
\usage{
linear_model_py_hw4b(form, d, contrasts = NULL)
}
\arguments{
\item{form}{linear model formula}

\item{d}{data frame}

\item{contrasts}{optional list of constants for factor variables aka contrast}
}
\value{
the best-fit coefficients for linear regression
}
\description{
This function fits linear models by calculating the coefficients
of regressions. Language is in Python 3.
}
\examples{
data(iris)
linear_model_py_hw4b(Sepal.Length ~ ., iris[1:50, -5])

}
