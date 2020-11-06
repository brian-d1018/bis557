#' @title Fitting Linear Model Function
#' @description This function fits linear models by calculating regressions.
#'
#' @param form linear model formula
#' @param d data frame
#' @param contrasts optional list of constants for factor variables aka contrast
#'
#' @return the best-fit coefficients for linear regression
#'
#' @examples
#' data(iris)
#' linear_model(Sepal.Length ~ ., iris)
#' linear_model(Sepal.Length ~ ., iris,
#'              contrasts = list(Species = "contr.sum"))
#'
#' @import stats
#' @export

linear_model <- function(form, d, contrasts=NULL) {
  if (is.null(contrasts) == TRUE) {
    # build the model matrix
    X <- model.matrix(form, d)
    y <- get_all_vars(form, d)[,1]
  } else {
    X <- model.matrix(form, d, contrasts.arg = contrasts)
    y <- get_all_vars(form, d)[,1]
  }

  # finding the coefficients depending on condition number
  if (kappa(X) < 1e5) {
    return(list("coefficients" = solve(crossprod(X), t(X) %*% y)))
  } else {
    return(list("coefficients" = qr.coef(qr(X), y)))
  }
}
