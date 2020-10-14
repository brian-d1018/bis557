library(testthat)

context("Test the output of homework 3c - multi-class logistic regression.")

test_that("Your multiclass_hw3c() function is 96% accurate for Iris data
          (3 classes).", {

            data(iris)

            iris_species_pred <- multiclass_hw3c(X = iris[,-5],
                                                 y = iris$Species,
                                                 maxiter = 60)

            expect_equivalent(mean(iris$Species == iris_species_pred), 0.96,
                              tolerance = 1e-2)
          })

test_that("Your multiclass_hw3c() function is 92% accurate for predicting the
          US region of a state (4 classes/regions).", {

            X <- state.x77[,c("Income", "Illiteracy", "Life Exp",
                              "Murder", "HS Grad", "Frost")]

            state_region_pred <- multiclass_hw3c(X = X, y = state.region,
                                                 maxiter = 40)

            expect_equivalent(mean(state.region == state_region_pred), 0.92,
                              tolerance = 1e-2)
          })
