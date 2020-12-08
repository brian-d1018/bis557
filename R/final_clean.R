#' @title Final - Cleaning Boston Dataset
#' @description This function is specific to the Final dataset for Boston
#' properties. This cleans the raw Boston dataset so that only residential
#' properties are analyzed. Descriptions are inside the function code.
#'
#' @return Cleaned Boston dataset
#'
#' @import stats
#' @import utils
#' @export

final_clean <- function() {
  # Load raw Boston dataset
  x <- bis557::final_data_bos

  # Residential Properties: Remove columns starting with `S_` or `U_`
  # Remove condos
  x <- x[, !colnames(x) %in% grep("^U_|^S_", colnames(x), value = TRUE)]

  # Remove owner info
  x <- x[, !colnames(x) %in% c("CM_ID", "GIS_ID", "OWN_OCC", "OWNER")]

  # Remove mailing address
  x <- x[, !colnames(x) %in% grep("^MAIL", names(x), value = TRUE)]

  # Type of property (land use): only residential, not commercial
  x <- x[x$LU %in% grep("^R", unique(x$LU), value = TRUE),]

  # Remove structure class
  x <- x[, !colnames(x) == "STRUCTURE_CLASS"]

  # Year Remodeled: change NA to `0`
  x$YR_REMOD[is.na(x$YR_REMOD)] <- 0

  # Make number of kitchens numeric
  x$R_KITCH <- gsub("^N - None$", 0, x$R_KITCH)
  x$R_KITCH <- gsub("^F - Full Eat In$", 1, x$R_KITCH)
  x$R_KITCH <- gsub("^O - One Person$", 1, x$R_KITCH)
  x$R_KITCH <- gsub("^$", 0, x$R_KITCH)
  x$R_KITCH <- as.integer(substr(x$R_KITCH, start = 1, stop = 1))

  # Delete rows with any `NA` data (this is why we changed NA to 0 for a column)
  x <- x[complete.cases(x),]

  # Zipcode: character displayed with 5 digits
  x$ZIPCODE <- sprintf("%05d", x$ZIPCODE)

  # Delete the zipcode `02186`
  x <- x[-which(x$ZIPCODE == "02186"),]

  # New variable for most recent upgrade (or if no upgrade, then year built)
  x$YR_LAST_UPGRA <- ifelse(x$YR_REMOD == 0, x$YR_BUILT, x$YR_REMOD)
  x$YR_REMOD <- NULL

  # New variable of price/value per square feet
  x$AV_TOTAL_SQFT <- x$AV_TOTAL/x$LIVING_AREA

  # Uppercase column names
  colnames(x) <- toupper(colnames(x))

  # Return cleaned data!
  return(x)
}
