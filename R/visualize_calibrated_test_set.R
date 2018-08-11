#' @title visualize_calibrated_test_set
#' @description plots a panel for all calibrated predictions from the respective calibration model. Allows visual comparison of the models output and their optimal cut off
#' @param actual vector of observed class labels (0/1)
#' @param predicted_list predict_calibratR$predictions object (list of calibrated predictions from calibration models)
#' @param cutoffs vector of optimal cut-off thresholds for each calibration model
#' @return ggplot2 element for visual comparison of the evaluated calibration models
#' @seealso
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{geom_point}},\code{\link[ggplot2]{scale_colour_manual}},\code{\link[ggplot2]{xlab}},\code{\link[ggplot2]{ylab}},\code{\link[ggplot2]{geom_hline}},\code{\link[ggplot2]{ylim}}
#' @rdname visualize_calibrated_test_set
#' @importFrom ggplot2 ggplot geom_point scale_colour_manual xlab ylab geom_hline ylim

visualize_calibrated_test_set <- function(actual, predicted_list, cutoffs){
  plots <- list()
  d <- data.frame(predicted_list)
  d$original <- NULL


  plot1 <- ggplot2::ggplot()+
    ggplot2::geom_point(ggplot2::aes(x=seq(1, length(actual)),y=predicted_list$original, colour=as.factor(actual)), show.legend = FALSE)+
    ggplot2::scale_colour_manual(values=c("darkolivegreen4","firebrick3"),name="Group",labels=c("Control","Case"))+
    ggplot2::xlab(label="idx") +
    ggplot2::ylab(label="original")+
    ggplot2::geom_hline(yintercept = 0.6, colour="black", linetype=3, size=0.7)+
    ggplot2::geom_hline(yintercept = 0.4, colour="black", linetype=3, size=0.7)+
    ggplot2::geom_hline(yintercept = cutoffs[[1]], linetype=4, colour="red")

  plots$original <- plot1

  for (i in names(d)){
    plot <- ggplot2::ggplot(data = d)+
      ggplot2::geom_point(ggplot2::aes_string(x=seq(1, length(actual)),y=i, colour=as.factor(actual)), show.legend = FALSE)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4","firebrick3"),name="Group",labels=c("Control","Case"))+
      ggplot2::xlab(label="idx") +
      ggplot2::ylim(c(0,1))+
      ggplot2::ylab(label=i)+
      ggplot2::geom_hline(yintercept = 0.6, colour="black", linetype=3, size=0.7)+
      ggplot2::geom_hline(yintercept = 0.4, colour="black", linetype=3, size=0.7)+
      ggplot2::geom_hline(yintercept = cutoffs[[i]], linetype=4, colour="red")
    plots[[i]] <- plot
  }

  if (any(sapply(plots, is.null))){
    plots <- plots[-which(sapply(plots, is.null))]
  }

  return(plots)
}
