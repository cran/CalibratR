#' @title plot_model
#' @description this methods visualizes all implemented calibration models as a mapping function between original ML scores (x-axis) and
#' calibrated predictions (y-axis)
#' @param calibration_model output from the \code{\link{calibrate}} method.
#' @param seq sequence of ML scores over which the mapping function should be evaluated, Default: 100 scores from the minimum to the maximum of the original ML scores
#' @return ggplot object
#' @seealso
#'  \code{\link[reshape2]{melt}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{geom_line}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{ylim}},\code{\link[ggplot2]{scale_colour_manual}},\code{\link[ggplot2]{theme}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{geom_text}},\code{\link[ggplot2]{geom_vline}}
#' @rdname plot_model
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_line aes ylim scale_colour_manual theme labs geom_text geom_vline

plot_model <- function(calibration_model, seq=NULL){
###local function###
  back_to_ML_scores <- function(midpoints, seq, score){
    idx <- c()
    for (i in 1:length(midpoints)){
      idx[i] <- which.min(abs(midpoints[i]-score))
    }
    return(seq[idx])
  }
  max <- max(calibration_model$original_values$predicted)
  min <- min(calibration_model$original_values$predicted)


  #default: if no seq is given, evaluate from min to max value of original input score
  if(is.null(seq)){
    step_size <- (max-min)/100 #evaluate 100 scores
    seq <- seq(min, max, step_size)
  }

  prediction_all <- predict_calibratR(calibration_model, seq, nCores=1)
  scaled  <- scale_me(seq)
  transformed <- transform_me(seq, mean=mean(seq))
  idx <- 1
  plot.list <- list()
  Var2 <- NULL
  value <- NULL


  for(i in prediction_all){
    control <- 1-i
    df <- cbind(seq,reshape2::melt(cbind(i, control), measure.vars=c("i","control")))
    plot_ori <- ggplot2::ggplot()+
      ggplot2::geom_line(data=data.frame(df),mapping=ggplot2::aes(x=seq, y=value, colour=Var2))+
      ggplot2::ylim(0, 1.05)+
      ggplot2::scale_colour_manual(values=c("firebrick3", "darkolivegreen4"), name="Group",labels=c("Case","Control") )+
      ggplot2::theme(legend.position = "right")+
      ggplot2::labs(title=names(prediction_all$predictions)[idx], x = "original ML score", y = "calibrated prediction")

      #add model specifc significance values
    model_pred <- names(prediction_all)[idx]

    if(model_pred=="BBQ_scaled_sel"|model_pred=="BBQ_scaled_avg"){
      z <- back_to_ML_scores(calibration_model$models$BBQ_scaled$binnning_scheme$midpoint, seq, scaled)
      plot.list[[model_pred]] <- plot_ori +
        ggplot2::geom_text(mapping=ggplot2::aes(x=as.numeric(z),
                              y=as.numeric(calibration_model$models$BBQ_scaled$binnning_scheme$prob_case)+0.03, label=(calibration_model$models$BBQ_scaled$binnning_scheme$significance)))

    }
    else if(model_pred=="BBQ_transformed_sel"|model_pred=="BBQ_transformed_avg"){
      z <- back_to_ML_scores(calibration_model$models$BBQ_transformed$binnning_scheme$midpoint, seq, transformed)
      plot.list[[model_pred]] <- plot_ori +
        ggplot2::geom_text(mapping=ggplot2::aes(x=as.numeric(z),
                              y=as.numeric(calibration_model$models$BBQ_transformed$binnning_scheme$prob_case)+0.03, label=calibration_model$models$BBQ_transformed$binnning_scheme$significance))

    }
    else if (model_pred=="hist_scal"){
      z <- back_to_ML_scores(calibration_model$models$hist_scaled$histogram$mids, seq, scaled)
      plot.list[[model_pred]] <- plot_ori +
        ggplot2::geom_text(mapping=ggplot2::aes(x=as.numeric(z),
                              y=as.numeric(calibration_model$models$hist_scaled$probs_per_bin)+0.03, label=calibration_model$models$hist_scaled$binnning_scheme$significance))
    }
    else if (model_pred=="hist_trans"){
      z <- back_to_ML_scores(calibration_model$models$hist_transformed$histogram$mids, seq, transformed)
      plot.list[[model_pred]] <- plot_ori +
        ggplot2::geom_text(mapping=ggplot2::aes(x=as.numeric(z),
                              y=as.numeric(calibration_model$models$hist_transformed$probs_per_bin)+0.03, label=calibration_model$models$hist_transformed$binnning_scheme$significance))
    }
    else if (model_pred=="GUESS_1"|model_pred=="GUESS_2"){
      if (calibration_model$models$GUESS$t_crit[1]>calibration_model$models$GUESS$t_crit[4]){
        plot.list[[model_pred]] <- plot_ori +
          ggplot2::geom_vline(mapping=ggplot2::aes(xintercept=calibration_model$models$GUESS$t_crit[2]), size=1, linetype=2)+
          ggplot2::geom_vline(mapping=ggplot2::aes(xintercept=calibration_model$models$GUESS$t_crit[3]), size=1, linetype=2)+
          ggplot2::geom_vline(mapping=ggplot2::aes(xintercept=calibration_model$models$GUESS$t_crit[1]), size=1, linetype=2)+
          ggplot2::geom_vline(mapping=ggplot2::aes(xintercept=calibration_model$models$GUESS$t_crit[4]), size=1, linetype=2)+
          ggplot2::geom_text(ggplot2::aes(x=calibration_model$models$GUESS$t_crit[2], y=0.9,label=("significance\n boundaries")), cex=0.6)
      }
      else
        plot.list[[model_pred]] <- plot_ori +
          ggplot2::geom_vline(mapping=ggplot2::aes(xintercept=calibration_model$models$GUESS$t_crit[2]), size=1, linetype=2)+
          ggplot2::geom_vline(mapping=ggplot2::aes(xintercept=calibration_model$models$GUESS$t_crit[3]), size=1, linetype=2)+
          ggplot2::geom_text(ggplot2::aes(x=calibration_model$models$GUESS$t_crit[2], y=0.9,label=("significance\n boundaries")), cex=0.6)
    }
    else
      plot.list[[model_pred]] <- plot_ori

    idx <- idx+1
  }

  return(plot.list)
  }
