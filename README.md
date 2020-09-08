---
output:
  pdf_document: default
  html_document: default
---

# bis557

<!-- badges: start -->
<!-- badges: end -->

The goal of {bis557} is to show the work that Brian did in the BIS 557 Fall 2020 
class. 
These include packages, functions, and data for common statistical algorithms.

## Installation

You can install the released version of bis557 from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("bis557")
```

## Dataset

Below is an example dataset, `lm_patho` from the {bis557} package, that can be 
used for linear regression.

``` r
library(bis557)
data(lm_patho)
head(lm_patho)
```

## Example: Linear Model

This is a basic example which shows you to create a linear model, for example, 
doing regression analysis. 

$$
y \sim \beta_1 x_1 + \beta_2 x_2
$$

<br><br>

``` r
library(bis557)
data(lm_patho)
fit_linear_model <- linear_model(y ~., lm_patho)
```

## Example: Gradient Descent

This is a basic example which shows you how to solve a common problem: Use an 
optimization algorithm, such as gradient descent, to find the coefficients of 
simple linear regression. 

<br><br>

``` r
library(bis557)
data(lm_patho)
gd_patho <- grad_descent(X = lm_patho[,-1], y = lm_patho[,1],
                         b_0 = rep(1e-16, ncol(lm_patho)), learn_rate = 1.3e-16,
                         max_iter = 1e5)
gd_patho
```
# bis557
