## code to prepare `lm_patho` dataset goes here
lm_patho <- read.csv("data-raw/lm_patho.csv", as.is = TRUE)

usethis::use_data(lm_patho, overwrite = TRUE)
