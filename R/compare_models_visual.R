#' @title compare_models_visual
#' @description FUNCTION_DESCRIPTION
#' @param models PARAM_DESCRIPTION
#' @param seq sequence for which the calibrated predictions should be plotted, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{geom_line}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{ylim}},\code{\link[ggplot2]{theme}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{scale_color_brewer}}
#'  \code{\link[reshape2]{melt}}
#' @rdname compare_models_visual
#' @importFrom ggplot2 ggplot geom_line aes ylim theme labs scale_color_brewer
#' @importFrom reshape2 melt



compare_models_visual <- function(models, seq=NULL){

  max <- max(models$original_values$predicted)
  min <- min(models$original_values$predicted)

  #default: if no seq is given, evaluate from min to max value of original input score
  if(is.null(seq)){
    step_size <- (max-min)/100 #evaluate 100 scores
    seq <- seq(min, max, step_size)
  }

  predictions <- predict_calibratR(models, seq, nCores=1)
  predictions$original <- NULL
  L1 <- NULL
  value <- NULL
  plot1 <- ggplot2::ggplot(cbind(seq,reshape2::melt(predictions)))+
    ggplot2::geom_line(ggplot2::aes(x=seq, y=value, colour=L1), size=1)+
    ggplot2::ylim(0, 1)+
    ggplot2::theme(legend.position = "bottom")+
    ggplot2::labs(title="Comparison of Calibration models",  x = "original ML score", y = "calibrated prediction")+
    ggplot2::scale_color_brewer(palette = "Paired", name=NULL)

   return(plot1)
}
