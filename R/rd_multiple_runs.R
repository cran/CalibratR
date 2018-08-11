#' @title rd_multiple_runs
#' @description This functions plots all n reliability diagrams that were constructed during n-times repeated m-fold cross-validation (CV).
#' During calibration model evaluation, CV is repeated n times, so that eventually n reliability diagrams are obtained.
#' @param list_models list object that contains n-times the output from the \code{\link{reliability_diagramm}}. method.
#' @return a list object that contains a reliability diagram that visualises all reliabilty diagrams that were constructed during n-times repeated m-fold cross-validation.
#' @seealso
#'  \code{\link[reshape2]{melt}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{geom_line}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_abline}},\code{\link[ggplot2]{ylab}},\code{\link[ggplot2]{xlab}},\code{\link[ggplot2]{xlim}},\code{\link[ggplot2]{ylim}},\code{\link[ggplot2]{coord_fixed}},\code{\link[ggplot2]{geom_text}},\code{\link[ggplot2]{scale_color_discrete}},\code{\link[ggplot2]{ggtitle}}
#' @rdname rd_multiple_runs
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_line aes geom_abline ylab xlab xlim ylim coord_fixed geom_text scale_color_discrete ggtitle
#' @importFrom stats median

rd_multiple_runs <- function(list_models){

list_bins <- list()
list_bins[["mean_prediction"]] <- list()
list_bins[["accuracy"]] <- list()
list_bins[["significance"]] <- list()

for (j in list_models){
    for (i in seq(1,10,1)){
      list_bins[["mean_prediction"]][[as.character(i)]] <- c(list_bins[["mean_prediction"]][[as.character(i)]],j$error$mean_pred_per_bin[[i]])
      list_bins[["accuracy"]][[as.character(i)]] <- c(list_bins[["accuracy"]][[as.character(i)]],j$error$accuracy_per_bin[[i]])
      list_bins[["significance"]][[as.character(i)]] <- c(list_bins[["significance"]][[as.character(i)]],j$error$sign[[i]])
    }
  }
  mean_pred <- data.frame(list_bins$mean_prediction)
  accuracy <- data.frame(list_bins$accuracy)
  significance <- data.frame(list_bins$significance)

    x <- reshape2::melt(t(mean_pred))
    y <- reshape2::melt(t(accuracy))
    df <- cbind(x,acc=y[,3])

    plot1 <- ggplot2::ggplot()+
      ggplot2::geom_line(data=df, ggplot2::aes(x=df$value, y=df$acc, group=df$Var2),
                         colour="grey70",alpha=0.3, size=1)+
      ggplot2::geom_line(ggplot2::aes(apply(mean_pred,2, FUN=median),
                                      apply(accuracy,2, FUN=median)), colour="#0072B2", size=2)+
      ggplot2::geom_abline(slope=1, color="#999999", size=1, linetype=2)+
      ggplot2::ylab("observed frequency")+
      ggplot2::xlab("mean prediction per bin")+
      ggplot2::xlim(0, 1) +
      ggplot2::ylim(0, 1.01) +
      ggplot2::coord_fixed(ratio=1)+
      ggplot2::geom_text(ggplot2::aes(x=unlist(mean_pred), y=unlist(accuracy),
                                      label=unlist(significance)), size=3.5, alpha=0.5)+
      ggplot2::scale_color_discrete(guide=FALSE)+
      ggplot2::ggtitle(paste("Reliability Diagrams from", nrow(accuracy),"partitions"))

    return(plot1)
}
