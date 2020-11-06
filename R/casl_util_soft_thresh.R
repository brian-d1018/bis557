#' @title Soft threshold function
#' @description Soft threshold function. This function is from the BIS 557
#' Textbook (Chapter 7, Page 189-190).
#'
#' @param a Numeric vector of values to threshold
#' @param b The soft thresholded value
#'
#' @return Numeric vector of the soft-thresholded values of a.
#'
#' @export

casl_util_soft_thresh <- function(a, b) {
  a[abs(a) <= b] <- 0
  a[a > 0] <- a[a > 0] - b
  a[a < 0] <- a[a < 0] + b
  return(a)
}
