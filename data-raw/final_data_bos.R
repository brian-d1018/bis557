## code to prepare `final_data_bos` dataset goes here
final_data_bos <- read.csv("data-raw/final_data_bos.csv", as.is = TRUE)

usethis::use_data(final_data_bos, overwrite = TRUE)
