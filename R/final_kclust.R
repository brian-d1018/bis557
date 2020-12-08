#' @title Final - K-Means Clustering of Zipcodes
#' @description This function is specific to the Final dataset for Boston
#' properties, which groups clusters into k clusters.
#'
#' @return The aggregated/scaled dataset and the cluster number of each zipcode
#'
#' @param x The cleaned Boston dataset
#' @param k Number of clusters
#'
#' @import stats
#' @export

final_kclust <- function(x, k) {
  set.seed(2020)
  # Keep only columns that are numeric
  x_zip <- x[, which(sapply(x, class) %in% c("integer", "numeric"))]

  # Include zipcode data on first column
  x_zip <- cbind(x$ZIPCODE, x_zip)
  colnames(x_zip)[1] <- "ZIPCODE"

  # Delete numeric columns that are geographic in nature
  # Delete numeric columns that are linear combinations of `AV_BLDG` & `AV_LAND`
  x_zip <- x_zip[, -which(colnames(x_zip) %in% c("PID", "PTYPE", "AV_TOTAL",
                                                 "GROSS_TAX", "AV_TOTAL_SQFT"))]

  # Scale columns (except zipcode) - take mean of all values
  x_zip[,-1] <- scale(x_zip[,-1])
  x_zip <- aggregate(x_zip[,-1], list(Zipcode = x_zip$ZIPCODE), FUN = "mean")

  # Clustering Tree: Euclidean Distance and Ward's Linkage
  hc <- hclust(d = dist(x_zip[,-1], "euclidean"), method = "ward.D2")

  # k clusters total
  x_cuts <- cutree(tree = hc, k = k)
  names(x_cuts) <- x_zip$Zipcode
  rownames(x_zip) <- x_zip$Zipcode

  ans <- list("x_zip" = x_zip, "x_cuts" = x_cuts)
  return(ans)
}
