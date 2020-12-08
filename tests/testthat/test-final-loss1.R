library(testthat)

context("Test my designed loss function with user-input weights.")

test_that("Your final_loss1() function is accurate.", {

  lossA <- final_loss1(y = 7, y_hat = c("1" = 5), w = c(2, 3),
                      df = data.frame(ZIP_GROUP = factor(2)))

  lossB <- final_loss1(y = c(7, 11, 4), y_hat = c("1" = 6, "2" = 7, "3" = 2),
                       w = c(0.5, 1.5, 1, 2),
                       df = data.frame(ZIP_GROUP = factor(c(4, 2, 1))))

  expect_equal(lossA, 8)

  expect_equal(lossB, 9)

})
