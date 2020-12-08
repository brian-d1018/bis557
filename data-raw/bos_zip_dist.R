## code to prepare `bos_zip_dist` dataset goes here
df <- final_clean()

# Sort all 30 zipcodes in order
z1 <- sort(unique(df$ZIPCODE))

# Figure out distance for each zipcode using Google Maps
z2 <- c(8, 0, 0, 0, 0, 0, 0, 0, 0, 2,
        2, 2, 4, 4, 4, 6, 0, 0, 0, 2,
        4, 6, 2, 4, 6, 0, 0, 2, 2, 4)

# Make data frame CSV
bos_zip_dist <- data.frame("ZIPCODE" = z1, "DIST_FROM_DT" = z2)
write.csv(bos_zip_dist, "data-raw/bos_zip_dist.csv", row.names = FALSE)
usethis::use_data(bos_zip_dist, overwrite = TRUE)
