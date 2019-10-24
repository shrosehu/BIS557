library(testthat)
library(MASS)

context("Test the output of Ridge Regression.")

test_that("You ridge_regression() function works.", {

  data(iris)

  fit_ridge <- ridge_regression(Sepal.Length ~ ., iris, lambda=0.5)

  fit_lm <- lm.ridge(Sepal.Length  ~ ., iris,lambda=0.5)

  expect_equivalent(coef(fit_lm), fit_ridge, tolerance = 1e-3)

})

test_that("You linear_model() function works with contrasts.", {

  data(iris)

  fit_ridge <- ridge_regression(Sepal.Length ~ ., iris, contrasts = list(Species = "contr.sum"))

  fit_lm <- lm.ridge(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"))

  expect_equivalent(coef(fit_lm), fit_ridge, tolerance = 1e-3)
})

test_that("Your linear_model() function works in a tougher case.", {

  data(lm_patho)

  fit_ridge <- ridge_regression(y ~., lm_patho)

  fit_lm <- lm.ridge(y ~., lm_patho)

  expect_equivalent(coef(fit_lm), fit_ridge, tolerance = 1e-3)
})
