#' @title getRMSE
#' @description calculates the root of mean square error (RMSE) in the test set of calibrated predictions
#' @param actual vector of observed class labels (0/1)
#' @param predicted vector of uncalibrated predictions
#' @return RMSE value
#' @rdname getRMSE


getRMSE <- function(actual, predicted){
  res <- (((actual-predicted)%*%(actual-predicted)/length(actual))^0.5)
  return(as.numeric(res))
}
