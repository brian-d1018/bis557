#' @title Fitting Linear Model Function - Python
#' @description This function fits linear models by calculating the coefficients
#' of regressions. Language is in Python 3.
#'
#' @param form linear model formula
#' @param d data frame
#' @param contrasts optional list of constants for factor variables aka contrast
#'
#' @return the best-fit coefficients for linear regression
#'
#' @examples
#' data(iris)
#' linear_model_py_hw4b(Sepal.Length ~ ., iris[1:50, -5])
#'
#' @import stats
#' @import reticulate
#' @export

linear_model_py_hw4b <- function(form, d, contrasts=NULL) {
  if (is.null(contrasts) == TRUE) {
    # build the model matrix
    X <- model.matrix(form, d)
    y <- get_all_vars(form, d)[,1]
  } else {
    X <- model.matrix(form, d, contrasts.arg = contrasts)
    y <- get_all_vars(form, d)[,1]
  }

  # Import NumPy
  np <- import("numpy", as = "np", convert = FALSE)
  qrs <- r_to_py(np$linalg$qr(X))
  q <- qrs[[0]]
  r <- qrs[[1]]
  b_hat <- py_to_r(np$linalg$inv(r)$dot(q$T)$dot(y))

  my_list <- list("coefficients" = b_hat)
  return(my_list)
}
