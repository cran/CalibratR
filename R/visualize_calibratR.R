#visualize
#' @title visualize_calibratR
#' @description this method offers a variety of visualisations to compare implemented calibration models
#' @author Johanna Schwarz
#' @param calibrate_object the list component \code{calibration_models} from the \code{\link{calibrate}} method
#' @param plot_distributions returns a density distribution plot of the calibrated predictions after CV (External) or without CV (internal)
#' @param rd_partitions returns a reliability diagram for each model
#' @param training_set_calibrated returns a list of ggplots. Each plot represents the calibrated predictions by the respective calibration model of the training set.
#' If the list object \code{predictions} in the \code{calibrate_object} is empty, \code{training_set_calibrated} is returned as NULL.
#' @param visualize_models returns the list components \code{plot_calibration_models} and \code{plot_single_models}
#' @return An object of class list, with the following components:
#' \item{histogram_distribution}{returns a histogram of the original ML score distribution}
#' \item{density_calibration_internal}{returns a list of density distribution plots for each calibration method, the original
#' and the two input-preprocessing methods scaling and transforming. The plot visualises the density distribution of the calibrated predictions of the training set. In this case, training and test set values are identical, so be careful to evaluate the plots.}
#' \item{density_calibration_external}{returns a list of density distribution plots for each calibration method, the original
#' and the two input-preprocessing methods scaling and transforming. The plot visualises the density distribution of the calibrated predictions, that were returned during Cross Validation. If more than one repetition of CV was performed,
#' run number 1 is evaluated}
#' \item{plot_calibration_models}{ maps the original ML scores to their calibrated prediction estimates for each model.
#' This enables easy model comparison over the range of ML scores See also \code{\link{compare_models_visual}}. }
#' \item{plot_single_models}{returns a list of ggplots for each calibration model, also mapping the original ML scores to their calibrated prediction. Significance values are indicated.
#' See also \code{\link{plot_model}}}
#' \item{rd_plot}{returns a list of reliability diagrams for each of the implemented calibration models and the two input-preprocessing methods "scaled" and "transformed". The returned plot visualises the calibrated predictions that
#' were returned for the test set during each of the n run of the n-times repeated CV. Each grey line represents one of the n runs. The blue line represents the median of all calibrated bin predictions.
#' Insignificant bin estimates are indicated with "ns". If no CV was performed during calibration model building using the \code{\link{calibrate}} method, \code{rd_plot} is returned as NULL}
#' \item{calibration_error}{returns a list of boxplots for the calibration error metrics ECE, MCE, CLE and RMSE. The n values for each model represent the obtained error values during the
#' n times repeated CV. If no CV was performed during calibration model building using the \code{\link{calibrate}} method, \code{calibration_error} is returned as NULL}
#' \item{discrimination_error}{returns a list of boxplots for the discrimination error AUC, sensitivity and specificity. The n values for each model represent the obtained error values during the
#' n times repeated CV. If no CV was performed during calibration model building using the \code{\link{calibrate}} method, \code{discrimination_error} is returned as NULL}
#' \item{cle_class_specific_error}{If no CV was performed during calibration model building using the \code{\link{calibrate}} method, \code{cle_class_specific_error} is returned as NULL}
#' \item{training_set_calibrated}{returns a list of ggplots. Each plot represents the calibrated predictions by the respective calibration model of the training set.
#' If the list object \code{predictions} in the \code{calibrate_object} is empty, \code{training_set_calibrated} is returned as NULL.}
#' \item{GUESS_1_final_model}{plots the the returned conditional probability p(x|Class) values of the GUESS_1 model}
#' \item{GUESS_2_final_model}{plots the the returned conditional probability p(x|Class) values of the GUESS_2 model}
#' @examples
#' ## Loading dataset in environment
#'  data(example)
#'  calibration_model <- example$calibration_model
#'
#'  visualisation <- visualize_calibratR(calibration_model, plot_distributions=FALSE,
#'  rd_partitions=FALSE, training_set_calibrated=FALSE)
#' @seealso
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{geom_density}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{scale_colour_manual}},\code{\link[ggplot2]{scale_fill_manual}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{geom_point}},\code{\link[ggplot2]{geom_hline}},\code{\link[ggplot2]{theme}},\code{\link[ggplot2]{element_text}}
#'  \code{\link[reshape2]{melt}}
#' @rdname visualize_calibratR
#' @export
#' @importFrom ggplot2 ggplot geom_density aes scale_colour_manual scale_fill_manual labs geom_point geom_hline theme element_text
#' @importFrom reshape2 melt

visualize_calibratR <- function(calibrate_object, visualize_models=FALSE, plot_distributions=FALSE, rd_partitions=FALSE, training_set_calibrated=FALSE){
  visualize_distributions <- TRUE
  visualize_errors_CV <- TRUE
  visualize_cle_class_error <- TRUE

  if(is.null(calibrate_object$calibration_models$models_final$GUESS)){
    visualize_guess <- FALSE
  }
  else{
    visualize_guess <- TRUE
    }
  training_set_calibrated <- TRUE

  if(length(calibrate_object$calibration_models$models_final)!=5){
    warning("Not all calibration models were trained. Certain visualisations may not be available. ")
  }

  if(is.null(calibrate_object$summary_CV$models$calibrated)){
    visualize_errors_CV <- FALSE
    plot_distributions <- FALSE
    rd_partitions <- FALSE
    visualize_cle_class_error <- FALSE
    warning("The list object summary_CV of the calibrate_object is empty. Certain visualisations may not be available. ")
  }

  if(is.null(calibrate_object$predictions)){
    plot_distributions <- FALSE
    training_set_calibrated <- FALSE
    warning("The list object predictions of the calibrate_object is empty. Certain visualisations may not be available.")

  }
  if(is.null(calibrate_object$summary_no_CV$discrimination_error)){
    training_set_calibrated <- FALSE
    warning("The list object summary_no_CV of the calibrate_object is empty. Certain visualisations may not be available.")
  }
  if(visualize_models){
    plot_models <- compare_models_visual(calibrate_object$calibration_models)
    plot_single_models <- plot_model(calibrate_object$calibration_models)
  }
  else{
    plot_models <- NULL
    plot_single_models <- NULL
    }

  if(plot_distributions){
    p0 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$original[calibrate_object$calibration_models$original_values$actual==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$original[calibrate_object$calibration_models$original_values$actual==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="Uncalibrated", x = "ML score")
    p1 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$scaled[calibrate_object$calibration_models$original_values$actual==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$scaled[calibrate_object$calibration_models$original_values$actual==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, internal, scaled", x = "uncalibrated prediction")
    p2 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$transformed[calibrate_object$calibration_models$original_values$actual==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$transformed[calibrate_object$calibration_models$original_values$actual==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, internal, transformed", x = "uncalibrated prediction")
    p3 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$hist_scal[calibrate_object$calibration_models$original_values$actual==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$hist_scal[calibrate_object$calibration_models$original_values$actual==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, internal, hist_scal", x = "calibrated prediction")
    p4 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$hist_trans[calibrate_object$calibration_models$original_values$actual==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$hist_trans[calibrate_object$calibration_models$original_values$actual==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, internal, hist_trans", x = "calibrated prediction")
    p5 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$BBQ_scaled_sel[calibrate_object$calibration_models$original_values$actual==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$BBQ_scaled_sel[calibrate_object$calibration_models$original_values$actual==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, internal, BBQ_scaled_sel", x = "calibrated prediction")
    p6 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$BBQ_scaled_avg[calibrate_object$calibration_models$original_values$actual==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$BBQ_scaled_avg[calibrate_object$calibration_models$original_values$actual==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, internal, BBQ_scaled_avg", x = "calibrated prediction")
    p7 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$BBQ_transformed_sel[calibrate_object$calibration_models$original_values$actual==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$BBQ_transformed_sel[calibrate_object$calibration_models$original_values$actual==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, internal, BBQ_transformed_sel", x = "calibrated prediction")
    p8 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$BBQ_transformed_avg[calibrate_object$calibration_models$original_values$actual==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$BBQ_transformed_avg[calibrate_object$calibration_models$original_values$actual==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, internal, BBQ_transformed_avg", x = "calibrated prediction")
    p9 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$GUESS_1[calibrate_object$calibration_models$original_values$actual==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$GUESS_1[calibrate_object$calibration_models$original_values$actual==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, internal, GUESS_1", x = "calibrated prediction")
    p10 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$GUESS_2[calibrate_object$calibration_models$original_values$actual==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$predictions$GUESS_2[calibrate_object$calibration_models$original_values$actual==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, internal, GUESS_2", x = "calibrated prediction")
    distribution_list <- list(original=p0,scaled=p1,transformed=p2,hist_scaled=p3,hist_transformed=p4,BBQ_scaled_sel=p5,
                              BBQ_scaled_avg=p6,BBQ_transformed_sel=p7,BBQ_transformed_avg=p8,GUESS_1=p9,GUESS_2=p10)
    p0 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$calibration_models$original_values$predicted[calibrate_object$calibration_models$original_values$actual==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$calibration_models$original_values$predicted[calibrate_object$calibration_models$original_values$actual==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="Uncalibrated", x = "ML score")
    p1 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$uncalibrated$scaled[[1]]$probs_CV[calibrate_object$summary_CV$models$uncalibrated$scaled[[1]]$actual_CV==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$uncalibrated$scaled[[1]]$probs_CV[calibrate_object$summary_CV$models$uncalibrated$scaled[[1]]$actual_CV==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, external, scaled", x = "uncalibrated prediction")
    p2 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$uncalibrated$transformed[[1]]$probs_CV[calibrate_object$summary_CV$models$uncalibrated$transformed[[1]]$actual_CV==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$uncalibrated$transformed[[1]]$probs_CV[calibrate_object$summary_CV$models$uncalibrated$transformed[[1]]$actual_CV==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, external, transformed", x = "uncalibrated prediction")
    p3 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$calibrated$hist_scaled[[1]]$probs_CV[calibrate_object$summary_CV$models$calibrated$hist_scaled[[1]]$actual_CV==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$calibrated$hist_scaled[[1]]$probs_CV[calibrate_object$summary_CV$models$calibrated$hist_scaled[[1]]$actual_CV==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, external, hist_scal", x = "calibrated prediction")
    p4 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$calibrated$hist_transformed[[1]]$probs_CV[calibrate_object$summary_CV$models$calibrated$hist_transformed[[1]]$actual_CV==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$calibrated$hist_transformed[[1]]$probs_CV[calibrate_object$summary_CV$models$calibrated$hist_transformed[[1]]$actual_CV==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, external, hist_trans", x = "calibrated prediction")
    p5 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$calibrated$BBQ_scaled_sel[[1]]$probs_CV[calibrate_object$summary_CV$models$calibrated$BBQ_scaled_sel[[1]]$actual_CV==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$calibrated$BBQ_scaled_sel[[1]]$probs_CV[calibrate_object$summary_CV$models$calibrated$BBQ_scaled_sel[[1]]$actual_CV==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, external, BBQ_scaled_sel", x = "calibrated prediction")
    p6 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$calibrated$BBQ_scaled_avg[[1]]$probs_CV[calibrate_object$summary_CV$models$calibrated$BBQ_scaled_avg[[1]]$actual_CV==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$calibrated$BBQ_scaled_avg[[1]]$probs_CV[calibrate_object$summary_CV$models$calibrated$BBQ_scaled_avg[[1]]$actual_CV==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, external, BBQ_scaled_avg", x = "calibrated prediction")
    p7 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$calibrated$BBQ_transformed_sel[[1]]$probs_CV[calibrate_object$summary_CV$models$calibrated$BBQ_transformed_sel[[1]]$actual_CV==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$calibrated$BBQ_transformed_sel[[1]]$probs_CV[calibrate_object$summary_CV$models$calibrated$BBQ_transformed_sel[[1]]$actual_CV==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, external, BBQ_transformed_sel", x = "calibrated prediction")
    p8 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$calibrated$BBQ_transformed_avg[[1]]$probs_CV[calibrate_object$summary_CV$models$calibrated$BBQ_transformed_avg[[1]]$actual_CV==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$calibrated$BBQ_transformed_avg[[1]]$probs_CV[calibrate_object$summary_CV$models$calibrated$BBQ_transformed_avg[[1]]$actual_CV==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, external, BBQ_transformed_avg", x = "calibrated prediction")
    p9 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$calibrated$GUESS_1[[1]]$probs_CV[calibrate_object$summary_CV$models$calibrated$GUESS_1[[1]]$actual_CV==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$calibrated$GUESS_1[[1]]$probs_CV[calibrate_object$summary_CV$models$calibrated$GUESS_1[[1]]$actual_CV==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, external, GUESS_1", x = "calibrated prediction")
    p10 <- ggplot2::ggplot()+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$calibrated$GUESS_2[[1]]$probs_CV[calibrate_object$summary_CV$models$calibrated$GUESS_2[[1]]$actual_CV==0], colour="darkolivegreen4", fill="darkolivegreen4"),alpha=0.2)+
      ggplot2::geom_density(ggplot2::aes(x=calibrate_object$summary_CV$models$calibrated$GUESS_2[[1]]$probs_CV[calibrate_object$summary_CV$models$calibrated$GUESS_2[[1]]$actual_CV==1], colour="firebrick3", fill="firebrick3"),alpha=0.2)+
      ggplot2::scale_colour_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::scale_fill_manual(values=c("darkolivegreen4", "firebrick3"),labels=c("control", "case"), name="Group")+
      ggplot2::labs(title="calibrated, external, GUESS_2", x = "calibrated prediction")
    distribution_list_external <- list(original=p0,scaled=p1,transformed=p2,hist_scaled=p3,hist_transformed=p4,BBQ_scaled_sel=p5,
                                       BBQ_scaled_avg=p6,BBQ_transformed_sel=p7,BBQ_transformed_avg=p8,GUESS_1=p9,GUESS_2=p10)
  }
  else{
    distribution_list <- NULL
    distribution_list_external <- NULL

  }

  if (visualize_distributions){
    plot_distribution <- visualize_distribution(calibrate_object$calibration_models$original_values$actual,
                                                calibrate_object$calibration_models$original_values$predicted)
  }
  else{
    plot_distribution <- NULL
  }

  if(rd_partitions){
    rd_plot <- list()
    rd_plot[["scaled"]] <- rd_multiple_runs(calibrate_object$summary_CV$models$uncalibrated$scaled)
    rd_plot[["transformed"]] <- rd_multiple_runs(calibrate_object$summary_CV$models$uncalibrated$transformed)
    rd_plot[["hist_scaled"]] <- rd_multiple_runs(calibrate_object$summary_CV$models$calibrated$hist_scaled)
    rd_plot[["hist_transformed"]] <- rd_multiple_runs(calibrate_object$summary_CV$models$calibrated$hist_transformed)
    rd_plot[["BBQ_scaled_sel"]] <- rd_multiple_runs(calibrate_object$summary_CV$models$calibrated$BBQ_scaled_sel)
    rd_plot[["BBQ_scaled_avg"]] <- rd_multiple_runs(calibrate_object$summary_CV$models$calibrated$BBQ_scaled_avg)
    rd_plot[["BBQ_transformed_sel"]] <- rd_multiple_runs(calibrate_object$summary_CV$models$calibrated$BBQ_transformed_sel)
    rd_plot[["BBQ_transformed_avg"]] <- rd_multiple_runs(calibrate_object$summary_CV$models$calibrated$BBQ_transformed_avg)
    rd_plot[["GUESS_1"]] <- rd_multiple_runs(calibrate_object$summary_CV$models$calibrated$GUESS_1)
    rd_plot[["GUESS_2"]] <- rd_multiple_runs(calibrate_object$summary_CV$models$calibrated$GUESS_2)
  }
  else{
    rd_plot <- NULL
  }

  if(visualize_errors_CV){
    plots_calibration <- visualize_error_boxplot(calibrate_object$summary_CV$error_models$calibration, discrimination = FALSE)
    plots_discrimination <- visualize_error_boxplot(calibrate_object$summary_CV$error_models$discrimination, discrimination = TRUE)
  }
  else{
    plots_calibration <- NULL
    plots_discrimination <- NULL
  }

  if(visualize_cle_class_error){
    cle_class_specific <- get_CLE_comparison(calibrate_object$summary_CV$error_models$calibration)
  }
  else{
    cle_class_specific <- NULL
  }


  if(visualize_guess){
    guess1 <- plot_class_distributions(calibrate_object$calibration_models$models_final$GUESS, 1)
    guess2 <- plot_class_distributions(calibrate_object$calibration_models$models_final$GUESS, 2)
  }
  else{
    guess1 <- NULL
    guess2 <- NULL
  }

  if(training_set_calibrated){
    plot1 <- visualize_calibrated_test_set(calibrate_object$calibration_models$original_values$actual, calibrate_object$predictions,
                                  calibrate_object$summary_no_CV$discrimination_error[,"cutoff"])
  }
  else{
    plot1 <- NULL
  }


  return(list(histogram_distribution=plot_distribution, density_calibration_internal=distribution_list, density_calibration_external=distribution_list_external, plot_calibration_models=plot_models, plot_single_models=plot_single_models,
              rd_plot=rd_plot, calibration_error=plots_calibration,discrimination_error=plots_discrimination, cle_class_specific_error=cle_class_specific,
              training_set_calibrated=plot1,
              GUESS_1_final_model=guess1, GUESS_2_final_model=guess2))
}
