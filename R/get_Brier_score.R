#' @title get_Brier_score
#' @description FUNCTION_DESCRIPTION
#' @param actual vector of observed class labels (0/1)
#' @param predicted vector of uncalibrated predictions
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname get_Brier_score

get_Brier_score <- function(actual, predicted){
  n <- length(actual)
  n_1 <- length(actual==1)
  n_0 <- length(actual==0)
  sum <- 0
  sum_0 <- 0
  sum_1 <- 0

  for (i in seq(1,n,1)){
  diff <- abs((predicted[i]-actual[i]))^2
  sum <- sum+diff
  if(actual[i]==0){
    sum_0 <- sum_0+diff
  }
  else
    if(actual[i]==1){
      sum_1 <- sum_1+diff
    }
  }
  return(list(brier=sum/n, brier_1=sum_1/n_1, brier_0=sum_0/n_0))

}
