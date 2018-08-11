#' @title reliability_diagramm
#' @description Reliability curves allow checking if the predicted probabilities of a
# binary classifier are well calibrated. This function returns two arrays
# which encode a mapping from predicted probability to empirical probability.
# For this, the predicted probabilities are partitioned into equally sized
# bins and the mean predicted probability and the mean empirical probabilties
# in the bins are computed. For perfectly calibrated predictions, both
# quantities whould be approximately equal (for sufficiently many test samples).
# Note: this implementation is restricted to binary classification.
# breaks default value = 10
#' @param actual vector of observed class labels (0/1)
#' @param predicted vector of uncalibrated predictions
#' @param bins number of bins in the reliability diagram, Default: 10
#' @param plot_rd should the reliability diagram be plotted, Default: TRUE
#' @return a list object containing the following elements
#' \item{calibration_error}{}
#' \item{discrimination_error}{}
#' \item{rd_breaks}{}
#' \item{histogram_plot}{}
#' \item{diagram_plot}{}
#' \item{mean_pred_per_bin}{}
#' \item{accuracy_per_bin}{}
#' \item{freq_per_bin}{}
#' \item{sign}{}
#' @seealso
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{stat_bin}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{scale_fill_manual}},\code{\link[ggplot2]{theme}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{geom_point}},\code{\link[ggplot2]{xlim}},\code{\link[ggplot2]{ylim}},\code{\link[ggplot2]{geom_abline}},\code{\link[ggplot2]{geom_line}},\code{\link[ggplot2]{geom_text}},\code{\link[ggplot2]{geom_label}},\code{\link[ggplot2]{coord_fixed}}
#' @rdname reliability_diagramm
#' @importFrom ggplot2 ggplot stat_bin aes scale_fill_manual theme labs geom_point xlim ylim geom_abline geom_line geom_text geom_label coord_fixed
#' @importFrom graphics hist
#' @export
reliability_diagramm <- function(actual, predicted, bins=10, plot_rd=TRUE){

  plot1 <- NULL
  plot2 <- NULL
  mean_pred_per_bin_ <- NULL
  accuracy_per_bin_ <- NULL
  freq_per_bin <- NULL

  #error values
  ece <- getECE(actual, predicted, bins)
  mce <- getMCE(actual, predicted, bins)
  rmse <- getRMSE(actual, predicted)
  ece_ <- get_ECE_equal_width(actual, predicted, bins)
  mce_ <- get_MCE_equal_width(actual, predicted, bins)
  cle <- get_CLE_class(actual, predicted, bins)
  brier <- get_Brier_score(actual, predicted)
  discrimination_error <- evaluate_discrimination(actual, predicted)

  #only plot reliability diagram if all(predicted) is between 0 and 1, komischer Rundungsfehler.... deshalb 1.0001
  if(all(predicted<=1.00001) && all(predicted>=0)){

    all <- data.frame(cbind(actual,predicted))
    histogram <- hist(all[,2],  breaks=seq(0,1,1/bins),plot=FALSE)
    accuracy_per_bin <- rep(0,length(histogram$mids))
    mean_pred_per_bin <- rep(0,length(histogram$mids))

   #sort predicted
    x <- order(predicted)
    predicted <- predicted[x]
    actual <- actual[x]

    for(i in 1:length(predicted)){
      for (j in 1:(length(histogram$breaks)-1)){
        if (predicted[i]==histogram$breaks[1]){ #values with prob = 0 are put in bin 1
          accuracy_per_bin[j] <- accuracy_per_bin[j] + actual[i]
          mean_pred_per_bin[j] <- mean_pred_per_bin[j] + predicted[i]
          break
        }
        if (histogram$breaks[j] < predicted[i] && predicted[i]<= histogram$breaks[j+1]){
          accuracy_per_bin[j] <- accuracy_per_bin[j] + actual[i]
          mean_pred_per_bin[j] <- mean_pred_per_bin[j] + predicted[i]
          break
        }
      }}

    mean_pred_per_bin_ <- mean_pred_per_bin/histogram$counts #mean prediction in bin
    mean_pred_per_bin_[is.nan(mean_pred_per_bin_)] <- 0
    accuracy_per_bin_ <- accuracy_per_bin/histogram$counts #no. of cases per bin
    accuracy_per_bin_[is.nan(accuracy_per_bin_)] <- 0
    pvalue_per_bin <- unlist(apply(cbind(success=accuracy_per_bin, all= histogram$counts),1,binom_for_histogram))
    freq_per_bin <- histogram$counts/sum(histogram$counts)
    sign <- c()

    for (i in (1: length(pvalue_per_bin))){
      if (pvalue_per_bin[i]<0.05){
        sign[i] <- "*"
      }
      else if (pvalue_per_bin[i]==2){ #empty bins are indicated with pvalue of 2
        sign[i] <- "x"
      }
      else
        sign[i] <- "ns"
    }

    idx <- sign=="x"

    if(plot_rd){
      ..count.. <- NULL
      plot1 <- ggplot2::ggplot(data=all)+
        ggplot2::stat_bin(mapping=ggplot2::aes(x=predicted, fill=factor(actual)),color="white",alpha=0.6,breaks=seq(0,1,1/bins), position="identity")+
        ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control","case"), name="Group")+
        ggplot2::theme(legend.position = "top")+
        ggplot2::stat_bin(data=subset(all,actual==0), ggplot2::aes(x=predicted,label=..count..), breaks=seq(0,1,1/bins), geom="text", position="identity", size=4)+
        ggplot2::stat_bin(data=subset(all,actual==1), ggplot2::aes(x=predicted,label=..count..), breaks=seq(0,1,1/bins), geom="text", position="identity", size=4)+
        ggplot2::labs(title="Constructed Histogram for Reliability Diagram", subtitle=paste("bins:",bins), x = "prediction", y = "observed frequency")


      plot2 <- ggplot2::ggplot(data=data.frame(cbind(mean_pred_per_bin_,accuracy_per_bin_)),ggplot2::aes(mean_pred_per_bin_, accuracy_per_bin_))+
        ggplot2::geom_point(shape=18,color="black", size=3)+
        ggplot2::xlim(0, 1) +
        ggplot2::ylim(0, 1.05) +
        ggplot2::geom_abline(slope=1, color="#999999", size=1, linetype=2)+
        ggplot2::geom_line(data=data.frame(cbind(mean_pred_per_bin_=mean_pred_per_bin_[!idx],accuracy_per_bin_=accuracy_per_bin_[!idx])),
                  color="#0072B2", size=2)+
        ggplot2::geom_text(mapping=ggplot2::aes(mean_pred_per_bin_, accuracy_per_bin_+0.04,label=sign))+
        ggplot2::geom_label(mapping=ggplot2::aes(0.2,0.9, label=paste(paste("n:",length(predicted)),"\n",
                                              paste("ECE:",round(ece_,4)),"\n",
                                              "ns = not significant\n",
                                              "x = empty bin")), size=2)+
        ggplot2::coord_fixed(ratio=1)+
        ggplot2::labs(title ="Reliability Diagram", subtitle=paste("bins:",bins),  x = "mean prediction in bin", y = "observed frequency")

      # plot2 <- ggplot(data=data.frame(cbind(histogram$mids,accuracy_per_bin_)),aes(V1, accuracy_per_bin_))+
      #   geom_point(shape=18,color="black", size=3)+
      #   xlim(0, 1) + ylim(0, 1.05) +
      #   geom_abline(slope=1, color="#999999", size=1, linetype=2)+
      #   geom_line(data=data.frame(cbind(histogram$mids[!idx],accuracy_per_bin_=accuracy_per_bin_[!idx])),
      #             color="#0072B2", size=2)+
      #   geom_text(mapping=aes(V1, accuracy_per_bin_+0.04,label=sign))+
      #   geom_label(mapping=aes(0.2,0.9, label=paste(paste("n:",length(predicted)),"\n",
      #                                               paste("ECE:",round(ece_,4)),"\n",
      #                                               "ns = not significant\n",
      #                                               "x = empty bin")), size=2)+
      #   coord_fixed(ratio=1)+
      #   labs(title ="Reliability Diagram", subtitle=paste("bins:",breaks),  x = "bin midpoint", y = "observed frequency")
      #
    }}

  error_list <- list(ECE_equal_width=ece_, MCE_equal_width=mce_, ECE_equal_freq=ece, MCE_equal_freq=mce,
                     RMSE=rmse, CLE_class_1=cle$class_1, CLE_class_0=cle$class_0, brier=brier$brier,
                     brier_class_1=brier$brier_1, brier_class_0=brier$brier_0)
  rounded_list <- lapply(error_list,round,5)

return(list(calibration_error=rounded_list, discrimination_error=discrimination_error,
            rd_breaks=bins, histogram_plot=plot1, diagram_plot=plot2,
            mean_pred_per_bin=mean_pred_per_bin_, accuracy_per_bin=accuracy_per_bin_,
            freq_per_bin=freq_per_bin,
            sign=sign))
}

