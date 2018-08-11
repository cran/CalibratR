#' @title predict_hist_binning
#' @description predict for a new element using histogram binning
#' @param histogram the output of \code{\link{build_hist_binning}}
#' @param new vector of uncalibrated probabilities
#' @return a list object containing the following components
#' \item{predictions}{contains a vector of calibrated predictions}
#' \item{significance_test_set}{the percentage of \code{new} instances that was evaluated using significant prediction estimates}
#' \item{pred_per_bin}{a table containing the number of instances from \code{new} for each bin of the final binning scheme of \code{histogram}}
#' @rdname predict_hist_binning


predict_hist_binning <- function(histogram, new){

  breaks <- histogram$histogram$breaks
  bin_probs <- histogram$probs_per_bin
  out <- c()

  #percentage of significant predictions
  significant_bins <- subset(histogram$binnning_scheme$`no bin`, histogram$binnning_scheme$p_value<0.05)
  no_per_bin <- cut(new, breaks, labels = histogram$binnning_scheme$`no bin`,include.lowest = T)
  sign_test_set <- sum(table(no_per_bin)[significant_bins])/(sum(table(no_per_bin)))

  for(i in 1:length(new)){
    for (j in 1:(length(breaks)-1)){
      if (new[i]==breaks[1]){
        out[i] <- bin_probs[1]
      }
      if (breaks[j] < new[i] && new[i]<= breaks[j+1]){
        out[i] <- bin_probs[j]
      }
    }
  }

  return(list(predictions=out,significance_test_set=sign_test_set,pred_per_bin=table(no_per_bin)))
}
