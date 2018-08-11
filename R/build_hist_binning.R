#' @title build_hist_binning
#' @description calculate estimated probability per bin, input predicted and real score as numeric vector; builds a histogram binning model which can be used to calibrate uncalibrated predictions using the predict_histogramm_binning method
#' @param actual vector of observed class labels (0/1)
#' @param predicted vector of uncalibrated predictions
#' @param bins number of bins that should be used to build the binning model, Default: decide_on_break estimates optimal number of bins
#' @return returns the trained histogram model that can be used to calibrate a test set using the \code{\link{predict_hist_binning}} method
#' @rdname build_hist_binning
#' @details if trainings set is smaller then threshold (15 bins*5 elements=75), number of bins is decreased
#' @importFrom graphics hist
#' @importFrom stats na.omit


build_hist_binning <- function(actual, predicted, bins=NULL){
  ##local functions###
  decide_on_break <- function(predicted, breaks=15){

    #if trainingsset is smaller then threshold (15 bins*5 elements=75), decrease #bins depending on training set size
    if (length(predicted)<=75){
      breaks <- floor(length(predicted)/6)
    }

    #only use 0,1,by breaks, if input is between 0 and 1
    if(all(predicted<=1) && all(predicted>=0)){
      histogram <- hist(predicted, breaks=seq(0,1,(1/breaks)), plot=FALSE)
      #if there are more than 3  bins with insignificant (less then 18 elements) -> number of breaks is decreased
      while(sum(histogram$counts>0 & histogram$counts<18) > 3 && !breaks <= 7){
        breaks <- breaks-1
        histogram <- hist(predicted, breaks=seq(0,1,(1/breaks)), plot=FALSE)
      }}

    else {
      histogram <- hist(predicted, breaks=breaks, plot=FALSE)
      #if there are more than 3  bins with insignificant (less then 18 elements) -> number of breaks is decreased
      while(sum(histogram$counts>0 & histogram$counts<18) > 3 && !breaks <= 7){
        breaks <- breaks-1
        histogram <- hist(predicted, breaks=breaks, plot=FALSE)
      }}

    return(suggested_break_number=breaks)
  }

  if (is.null(bins)){
    n_bins <- decide_on_break(predicted)
  }
  else
    n_bins <- bins

  predicted_real <- data.frame()
  predicted_real[1:length(predicted),1] <- predicted
  predicted_real[,2] <- actual
  predicted_real <- na.omit(predicted_real)
  histogram <- hist(predicted_real[,1], breaks=seq(0,1,(1/n_bins)), plot=FALSE, include.lowest = T)

  true_bin <- data.frame()
  true_bin[c(1:length(histogram$count)),1] <- seq(1,length(histogram$count))
  true_bin[,2] <- 0 #true positiv
  true_bin[,3] <- 0 #no. of data in bin
  true_bin[,4] <- 0 #correct y=1 diagnosis
  colnames(predicted_real) <- c("ML score", "real score")

  for(i in 1:nrow(predicted_real)){
    for (j in 1:(length(histogram$breaks)-1)){
      if (predicted_real[i,1]==histogram$breaks[1]){ #values with prob = 0 are put in bin 1
        true_bin[1,3] <- true_bin[1,3] + 1
        true_bin[1,2] <- true_bin[1,2] + predicted_real[i,2]
        break
      }
      if (histogram$breaks[j] < predicted_real[i,1] && predicted_real[i,1]<= histogram$breaks[j+1]){
        true_bin[j,3] <- true_bin[j,3] + 1
        true_bin[j,2] <- true_bin[j,2] + predicted_real[i,2]
        break
      }
    }
  }

  true_bin[,4] <- true_bin[,2]/true_bin[,3] #Probability for correct y=1 diagnosis
  true_bin[,4][is.na(true_bin[,4])] <- 0

  #significance testing
  p_values_binom <- unlist(apply(cbind(true_bin[,2], true_bin[,3]),1,binom_for_histogram)) #pvalues for single bins, binom.test
  true_bin[,5] <- p_values_binom

  for (i in 1:nrow(true_bin)){
    if(is.nan(true_bin[i,4])){
      true_bin[i,6] <- "no value"
    }

    else if(true_bin[i,5]<0.001){
      true_bin[i,6] <- "***"
    }

    else if(true_bin[i,5]<0.01){
      true_bin[i,6] <- "**"
    }

    else if(true_bin[i,5]<0.05){
      true_bin[i,6] <- "*"
    }
    else
      true_bin[i,6] <- "ns"
  }

  colnames(true_bin) <- c("no bin", "true cases", "all", "prob_case", "p_value", "significance")
  colnames(predicted_real) <- c("ML score", "real score")

  min <- min(predicted)
  max <- max(predicted)

  #quality markers calibration model
  calibration_points <- true_bin[,4]
  calibration_points_sign <- true_bin[,5]<0.05
  calibration_points_number <- length((true_bin[,4]))
  calibration_points_number_sign <- length((subset(true_bin[,4], true_bin[,5]<0.05)))
  calibration_range <- range(true_bin[,4])
  if(sum(calibration_points_sign) != 0){
    calibration_range_sign <- range(true_bin[,4][true_bin[,5]<0.05])
  }
  else{
    calibration_range_sign <- 0
  }

  return(list(type="hist", histogram=histogram,probs_per_bin=true_bin[,4],
              binnning_scheme=true_bin, min=min, max=max,
              calibration_points=list(calibration_points=calibration_points,calibration_points_sign=calibration_points_sign),
              calibration_range=list(calibration_range=calibration_range, calibration_range_sign=calibration_range_sign),
              calibration_points_number=list(calibration_points_number=calibration_points_number, calibration_points_number_sign=calibration_points_number_sign)))
}
