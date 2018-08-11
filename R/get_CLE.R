#' @title get_CLE_class
#' @description calculates the class-specific classification error CLE in the test set.
#' The method computes the deviation of the calibrated predictions of class 1 instances from their true value 1.
#' For class 0 instances, \code{get_CLE_class} computes the deviation from 0.
#' Class 1 CLE is 0 when all class 1 instances have a calibrated prediction of 1 regardless of potential miscalibration of class 0 instances.
#' CLE calculation is helpful when miscalibration and -classification is more cost-sensitive for one class than for the other.
#' @param actual vector of observed class labels (0/1)
#' @param predicted vector of uncalibrated predictions
#' @param bins number of bins for the equal-width binning model, default=10
#' @return object of class list containing the following components:
#' \item{class_1}{CLE of class 1 instances}
#' \item{class_0}{CLE of class 0 instances}
#' @seealso
#'  \code{\link[reshape2]{melt}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{geom_line}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{position_dodge}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{scale_colour_manual}}
#' @rdname get_CLE_class
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_line aes position_dodge labs scale_colour_manual
#' @importFrom graphics hist

get_CLE_class <- function(actual, predicted, bins=10){ #equal width bins
  pred_actual <- cbind(predicted, actual)

  if(all(predicted<=1) && all(predicted>=0)){
    hist_x <- hist(pred_actual[,1], breaks=seq(0,1,1/bins), plot=F)
  }
  else{
    hist_x <- hist(pred_actual[,1], breaks=bins, plot=F)
  }

  breaks_y <- hist_x$breaks
  y_true <- hist(subset(pred_actual[,1], pred_actual[,2]=="1"), breaks=breaks_y, plot=F)
  divided <- cut(pred_actual[,1], breaks=c(hist_x$breaks), label = seq(1,length(y_true$mids)), include.lowest = T)
  divided_0 <- cut(pred_actual[,1][pred_actual[,2]==0], breaks=c(hist_x$breaks), label = seq(1,length(y_true$mids)), include.lowest = T)
  divided_1 <- cut(pred_actual[,1][pred_actual[,2]==1], breaks=c(hist_x$breaks), label = seq(1,length(y_true$mids)), include.lowest = T)

  prediction_in_bin <- list()
  expected <- c()
  prediction_in_bin_0 <- list()
  expected_0 <- c()
  prediction_in_bin_1 <- list()
  expected_1 <- c()

  for (i in as.numeric(levels(divided))){
    prediction_in_bin[[i]] <- pred_actual[which(divided==i),1]
    expected[i] <- mean(prediction_in_bin[[i]]) #mean prediction in that bin
    prediction_in_bin_0[[i]] <- subset(pred_actual,pred_actual[,2]==0)[which(divided_0==i),1]
    expected_0[i] <- mean(prediction_in_bin_0[[i]]) #mean prediction in that bin
    prediction_in_bin_1[[i]] <- subset(pred_actual,pred_actual[,2]==1)[which(divided_1==i),1]
    expected_1[i] <- mean(prediction_in_bin_1[[i]]) #mean prediction in that bin
  }


  counts_all <- hist_x$counts
  counts_true <- y_true$counts

  zeros <- which(counts_all==0)
  prevalence <- counts_true/counts_all
  prevalence[zeros] <- 0 #set prevalence to 0 when no observations are in the bin
  expected[zeros] <- hist_x$mids[zeros] #set expectation to the mid bin point, when no elements are in bin

  S_2 <- abs(prevalence-expected)
  W_2 <- counts_all/(length(predicted))
  expected_0[!is.finite(expected_0)] <- 0
  expected_1[!is.finite(expected_1)] <- 0

  S2_1 <- abs(1-expected_1)
  S2_0 <- abs(0-expected_0)

  #weighing adapted for  class 1
  W_2_1_all <- counts_true/(sum(pred_actual[,2]=="1")) #add up to 1
  #weighing adapted for  class 0
  W_2_0_all <- (counts_all-counts_true)/(sum(pred_actual[,2]=="0")) #add up to 1

  ECE_per_bin <- (S_2*W_2)
  ECE <- sum(ECE_per_bin)
  CLE_per_bin <- (S2_1*W_2_1_all)+(S2_0*W_2_0_all)
  CLE <- sum(CLE_per_bin)
  CLE_per_bin_1 <- (S2_1*W_2_1_all)
  CLE_1 <- sum(CLE_per_bin_1)
  CLE_per_bin_0 <- (S2_0*W_2_0_all)
  CLE_0 <- sum(CLE_per_bin_0)

  #Visualisation of CLE class errors
  bins_1 <- S2_1*W_2_1_all
  bins_0 <- S2_0*W_2_0_all

  # df <- reshape2::melt(cbind(CLE_class1=bins_1,CLE_class0=bins_0, prop_0=W_2_0_all, prop_1=W_2_1_all, ECE_all=ECE_per_bin))
  # plot1 <- ggplot2::ggplot()+
  #   ggplot2::geom_line(ggplot2::aes(x=df$Var1, y=(df$value), colour=df$Var2), position = ggplot2::position_dodge(width = 0.2))+
  #   ggplot2::labs(x="bin number", y="CLE")+
  #   ggplot2::scale_colour_manual(values=c("firebrick3", "darkolivegreen4", "cyan3", "grey", "black"), name = NULL)

  #show(plot1)

  return(list(class_1=as.numeric(t(S2_1)%*%W_2_1_all),
              class_0=as.numeric(t(S2_0)%*%W_2_0_all)))
}
