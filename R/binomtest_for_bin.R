#' @title binom_for_histogram
#' @description p_values from stats::binom.test for each bin, if bin is empty, a p-value of 2 is returned
#' @param n_x numeric vector of two integers. The first one is the number of cases in the bin; the second the number of instances in the bin
#' @return p-value from stats::binom.test method
#' @rdname binom_for_histogram
#' @importFrom stats binom.test

binom_for_histogram <- function(n_x){
  success <- n_x[1]
  all <- n_x[2]
  if(!(success==0 && all==0)){
    return(as.numeric(stats::binom.test(success,all)$p.value))
  }
  else #if bin is empty -> p-value of 2
    return(2)
}
