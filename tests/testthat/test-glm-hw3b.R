library(testthat)

context("Test the output of homework 3b - first order GLM.")

test_that("Your glm_hw3b() function works for a Poisson distributed
          response variable with the logarithm function as a link.", {

            set.seed(2020)

            n <- 3000; p <- 4; maxiter <- 500; steps <- rep(1e-3, maxiter)

            X <- cbind(1, matrix(rnorm(n * (p-1)), ncol = p-1))

            beta <- c(-1, 0.2, 0.1, 0.3)

            y <- rpois(n, lambda = exp(X %*% beta))

            beta_hat <- glm_hw3b(X, y, family = poisson(link = "log"),
                                 steps = steps, maxiter = maxiter)

            expect_equivalent(beta_hat$coefficients, c(-1.05, 0.20, 0.08, 0.29),
                              tolerance = 1e-1)
          })

test_that("Your glm_hw3b() function works for `airquality` ozone data using
          logistic regression.", {

            data(airquality)

            df <- na.omit(airquality)

            X <- as.matrix(df[,c("Solar.R", "Wind", "Temp", "Month")])

            y <- as.numeric(df$Ozone >= 50)

            maxiter <- 500; steps <- (1e0)/(1:maxiter)

            beta_hat <- glm_hw3b(X = X, y = y,
                                 family = binomial(link = "logit"),
                                 steps = steps, maxiter = maxiter)

            p <- 1 / (1 + exp(-X %*% beta_hat$coefficients))

            mean(y == round(p))

            expect_equivalent(round(x = mean(y == round(p)), 2), 0.87,
                              tolerance = 2e-2)

            expect_equivalent(beta_hat$coefficients, c(4, -710, 71, -114),
                              tolerance = 1e2)
          })

test_that("Your glm_hw3b() function works for a binomially distributed
          response variable with the Cauchy distribution as a link.", {

            set.seed(2020)

            n <- 700; p <- 3; maxiter <- 1e3; steps <- rep(1e-2, maxiter)

            X <- cbind(1, matrix(rnorm(n * (p-1)), ncol = p-1))

            beta <- c(0.4, -2, 0.5)

            y <- as.numeric(runif(n) > (1 - pcauchy(X %*% beta)))

            beta_hat <- glm_hw3b(X, y, family = binomial(link = "cauchit"),
                                 steps = steps, maxiter = maxiter)

            beta_hat

            expect_equivalent(beta_hat$coefficients, beta,
                              tolerance = 5e-1)
          })
