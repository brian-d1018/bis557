#' @title Fitting Linear Model Function
#' @description This function fits linear models by calculating regressions.
#'
#' @param formula linear model formula
#' @param data data frame
#' @param contrasts optional list of constants for factor variables aka contrast
#'
#' @examples
#' linear_model(Sepal.Length ~ ., iris)
#' linear_model(Sepal.Length ~ ., iris,
#'              contrasts = list(Species = "contr.sum"))
#'
#' @import stats
#' @export
#'

linear_model <- function(formula, data, contrasts=NULL) {
  # use the `lm` function
  if (is.null(contrasts)) {
    return(lm(formula, data))
  } else {
    return(lm(formula, data, contrasts=contrasts))
  }
}
