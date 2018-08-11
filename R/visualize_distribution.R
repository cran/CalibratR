#' @title visualize_distribution
#' @description FUNCTION_DESCRIPTION
#' @param actual vector of observed class labels (0/1)
#' @param predicted vector of uncalibrated predictions
#' @return list object containing the following components:
#' \item{plot_distribution}{ggplot histogram that visualizes the observed class distributions}
#' \item{parameter}{list object that summarizes all relevant parameters (mean, sd, number) of the observed class distributions}
#' @seealso
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{geom_histogram}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{scale_colour_manual}},\code{\link[ggplot2]{scale_fill_manual}},\code{\link[ggplot2]{labs}}
#' @rdname visualize_distribution
#' @importFrom ggplot2 ggplot geom_histogram aes scale_colour_manual scale_fill_manual labs
#' @importFrom stats sd


visualize_distribution <- function(actual, predicted){

  all <- data.frame(cbind(actual, predicted))
  n <- nrow(all)
  case <- subset(all, all$actual==1)
  control <- subset(all, all$actual==0)

  mean_cases <- mean(case$predicted)
  mean_control <- mean(control$predicted)
  sd_case <- sd(case$predicted)
  sd_control <- sd(control$predicted)
  n_cases <- nrow(case)
  n_control <- nrow(control)
  total <- n_cases+n_control

  plot_distribution <- ggplot2::ggplot()+
                          ggplot2::geom_histogram(ggplot2::aes(x=control$predicted, colour="darkolivegreen4", fill="darkolivegreen4"),bins=10,alpha=0.4)+
                          ggplot2::geom_histogram(ggplot2::aes(x=case$predicted, colour="firebrick3", fill="firebrick3"),bins=10,alpha=0.4)+
                          ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
                          ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
                          ggplot2::labs(title="Controls vs. Cases density, Test Set",y="Frequency", subtitle=paste("no. of cases", n_cases,"\n",
                                                                      "no. of controls", n_control),
                                                                      x = "original ML score")

  parameters <- c(mean_prediction_cases=mean_cases, mean_prediction_controls=mean_control,
                  sd_prediction_cases=sd_case, sd_prediction_controls=sd_control,
                  number_cases=n_cases, number_controls=n_control, total=total)

  return(list(plot_distribution=plot_distribution, parameter=parameters))
}
