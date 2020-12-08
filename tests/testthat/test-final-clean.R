library(testthat)

context("Test the output of final data cleaning function.")

test_that("Your final_clean() function cleans the final dataset correctly.", {

  x <- final_clean()

  expect_equal(dim(x), c(61327, 41))
})

test_that("Your final_clean() function possesses the correct columns.", {

  x <- final_clean()

  expect_equal(sum(sapply(x, class) == "character"), 22)
})
