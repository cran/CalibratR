#' @title plot_class_distributions
#' @description plots the the returned conditional class probabilities P(x|C) of GUESS_1 or GUESS_2 models. Which GUESS model is plotted can be specified in \code{pred_idx}.
#' @param build_guess_object output from build_GUESS()
#' @param pred_idx if \code{pred_idx}=1 GUESS_1 is plotted; if \code{pred_idx}=2 GUESS_2 is plotted
#' @return ggplot object that visualizes the returned calibrated predicition estimates by GUESS_1 or GUESS_2
#' @seealso
#'  \code{\link[reshape2]{melt}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{geom_line}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{scale_colour_manual}},\code{\link[ggplot2]{theme}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{geom_vline}},\code{\link[ggplot2]{geom_text}}
#' @rdname plot_class_distributions
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_line aes scale_colour_manual theme labs geom_vline geom_text


plot_class_distributions <- function(build_guess_object, pred_idx){

  min <- build_guess_object$min
  max <- build_guess_object$max
  x <- seq(min, max, 0.01)

  density <- predict_GUESS(build_guess_object, x, density_evaluation=pred_idx, TRUE)
  density_case <- density$dens_case
  density_controls <- density$dens_controls
  Var2 <- NULL
  value <- NULL

  df <- cbind(x,reshape2::melt(cbind(density_controls, density_case)))
  if(build_guess_object$t_crit[[1]]>build_guess_object$t_crit[[4]]){
    plot1 <- ggplot2::ggplot()+
      ggplot2::geom_line(data=data.frame(df),mapping=ggplot2::aes(x, y=value, colour=Var2))+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4","firebrick3"), name="Group",labels=c("Control","Case"))+
      ggplot2::theme(legend.position = "bottom")+
      ggplot2::labs(subtitle="Controls vs. Cases", x = "original ML score", y = "calibrated prediction")+
      ggplot2::geom_vline(mapping=ggplot2::aes(xintercept=build_guess_object$t_crit[[2]]), colour="grey", size=1, linetype=2)+
      ggplot2::geom_vline(mapping=ggplot2::aes(xintercept=build_guess_object$t_crit[[3]]), colour="grey", size=1, linetype=2)+
      ggplot2::geom_vline(mapping=ggplot2::aes(xintercept=build_guess_object$t_crit[[1]]), colour="grey", size=1, linetype=2)+
      ggplot2::geom_vline(mapping=ggplot2::aes(xintercept=build_guess_object$t_crit[[4]]), colour="grey", size=1, linetype=2)+
      ggplot2::geom_text(ggplot2::aes(x=build_guess_object$t_crit[[2]], y=max(df$value),label=("significance\n boundaries")),
                nudge_x=0.05,vjust = "inward", hjust = "inward", size=3)
  }
  else
  plot1 <- ggplot2::ggplot()+
    ggplot2::geom_line(data=data.frame(df),mapping=ggplot2::aes(x, y=value, colour=Var2))+
    ggplot2::scale_colour_manual(values=c("darkolivegreen4","firebrick3"), name="Group",labels=c("Control","Case"))+
    ggplot2::theme(legend.position = "bottom")+
    ggplot2::labs(subtitle="Controls vs. Cases", x = "original ML score", y = "calibrated prediction")+
    ggplot2::geom_vline(mapping=ggplot2::aes(xintercept=build_guess_object$t_crit[[2]]), colour="grey", size=1, linetype=2)+
    ggplot2::geom_vline(mapping=ggplot2::aes(xintercept=build_guess_object$t_crit[[3]]), colour="grey", size=1, linetype=2)+
    ggplot2::geom_text(ggplot2::aes(x=build_guess_object$t_crit[[2]], y=max(df$value),label=("significance\n boundaries")),
              nudge_x=0.05,vjust = "inward", hjust = "inward", size=3)
  return(plot1)
}
