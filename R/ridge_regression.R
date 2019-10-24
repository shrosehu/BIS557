#' ridge_regression
#'
#' ridge_regression is used to fit linear models. It can be used to address the problem of colinearity and overfitting.
#'
#' @param form A symbolic description of the model to be fitted.
#' @param data A data frame or list that contains the variables in the model.
#' @param lambda Penalty added to beta
#' @param contrasts a list of contrasts
#' @return The function returns a list of coefficients.
#' @examples
#' ridge_regression(Sepal.Length~ ., iris)
#'
#' @importFrom stats model.matrix
#'
#' @export


ridge_regression <- function(form, data, lambda = 0, contrasts=NULL) {
  rownames(data) <- NULL
  X <- model.matrix(form, data, contrasts)
  Y <- data[[as.character(form)[2]]][as.numeric(rownames(X))]

  mu_x <- colMeans(X[,-1])
  mu_y <- mean(Y)
  n <- nrow(X)
#rescaling to match the lm.ridge function results
  X <- X[,-1] - rep(mu_x, rep(n, ncol(X)-1))
  n_x <- ncol(X)
  Y <- Y - mu_y
  scaling <- drop(sqrt(rep(1/n, n)%*%(X^2)))
  X <- X/rep(scaling, rep(n, n_x))

  beta <- matrix(NA_real_, nrow=length(lambda),ncol=n_x)
  svd_decomp <- svd(X)
  beta <- (svd_decomp$v)%*%diag(svd_decomp$d/(svd_decomp$d^2 + lambda))%*%t(svd_decomp$u)%*%Y
  beta <- t(as.matrix(beta / scaling))
  beta_nought <- mu_y - beta%*%mu_x

  ret <- cbind(beta_nought, beta)
  returnme <- as.vector(ret)
  names(returnme) <- c("Intercept", colnames(X))
  returnme
}

#' the predict method
#'
#' @param object an object of ridge_regression type
#' @param ... `(dataframe)`
#'
#' @return a prediction of Y for the given X
#' @export
#'
#'
#predicting using ridge regression function
predict.ridge_regression <- function(object, ...) {
  dots <- list(...)
  x_frame <- dots[[1]]
  if (!is.data.frame(x_frame)) {
    stop("The first argument should be a data.frame of values",
             "to predict")
  }
  X <- model.matrix(attributes(object)$formula, x_frame)
  X %*% object
}
