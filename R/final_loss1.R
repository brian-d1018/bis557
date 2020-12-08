#' @title Final - Loss Function for Polynomial Model
#' @description This function is specific to the Final dataset for Boston
#' properties. This is my designed loss function for the polynomial model.
#'
#' @return The loss value
#'
#' @param y Actual target vector
#' @param y_hat Estimated/predicted target vector (with its `names` as the test index)
#' @param w Vector of weights
#' @param df The cleaned Boston dataset
#'
#' @import stats
#' @export

final_loss1 <- function(y, y_hat, w, df) {
  # Same length for the target vectors
  if (length(y) != length(y_hat)) stop("Target vectors need same length.")
  if (length(unique(df$ZIP_GROUP)) > length(w)) stop("Check number of clusters.")

  # Find the cluster groups for each test observation
  group_num <- df$ZIP_GROUP[as.numeric(names(y_hat))]

  # Make the weighted matrix's diagonal
  W <- w[group_num]

  # Mean loss function using dot products
  loss <- mean( W * (y - y_hat)^2 )

  return(loss)
}
