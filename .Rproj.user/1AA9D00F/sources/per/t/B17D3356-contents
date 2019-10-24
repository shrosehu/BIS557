#' optimizing_lambda
#'
#' optimizing_lambda is used to find the optimal lambda in ridge regression by using cross validation
#'
#' @param form A symbolic description of the model to be fitted.
#' @param data A data frame or list that contains the variables in the model.
#' @param lambdas Penalty added to beta
#' @param folds number of subsets of the data that is used in training and testing sample
#' @param contrasts a list of contrasts
#' @return The function returns a tibble that summarizes rmse and lambda
#'
#'
#' @importFrom magrittr %>%
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores
#' @importFrom rsample vfold_cv testing training
#' @importFrom foreach %do% %dopar% foreach
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @import stats
#' @export

optimizing_lambda <- function (form, data, folds=10, lambdas=seq(0,1,0.05), contrasts= NULL){

  i<- NULL
  lambda <- NULL
  `.`<- NULL
  lower <- NULL
  upper <- NULL

  folds <- vfold_cv(data,v=folds)

  registerDoParallel(1)

  #calculates mse
  casl_util_rmse <- function(y, y_hat){
    mu <- mean((y-y_hat)^2)
    mu
  }

  #calculates mse for each lambda each fold
  rmses <- foreach(lambda = lambdas, .combine = rbind, .packages = c("foreach")) %dopar% {
    foreach(i = seq_len(nrow(folds)), .combine = c, .packages = c("rsample", 'stats') ) %do% {
      casl_util_rmse((testing(folds$splits[[i]]))[[as.character(formula[2])]],
                     predict(ridge_regression(form, training(folds$splits[[i]]),lambda = lambda, contrasts= contrasts),
                             testing(folds$splits[[i]])))
    }
  }

  #find the lambda minimizes mse
  edf <- tibble(mean = apply(rmses, 1, mean), sd = apply(rmses, 1, sd), lambda = lambdas) %>%
    mutate(upper = mean + 2 * sd / sqrt(nrow(.)), lower = mean - 2 * sd / sqrt(nrow(.)))

  min_lambda <- edf$lambda[which.min(edf$mean)]
  list(min_lambda = min_lambda)
}
