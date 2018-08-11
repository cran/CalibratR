#' @title calibrate_me
#' @description trains calibration models on the training set of \code{predicted}/\code{actual} value pairs.\code{model_idx} specifies which models should be trained.
#' @param actual vector of observed class labels (0/1)
#' @param predicted vector of uncalibrated predictions
#' @param model_idx a single number from 1 to 5, indicating which calibration model should be implemented, 1=hist_scaled, 2=hist_transformed, 3=BBQ_scaled, 4=BBQ_transformed, 5=GUESS
#' @return depending on the value of \code{model_idx}, the respective calibration model is build on the input from \code{actual} and \code{predicted}
#' @rdname calibrate_me

calibrate_me <- function(actual, predicted, model_idx){

  if (length(predicted)<=75){ #If input set is too small, bin no. in rd is decreased for all (!) models
    breaks_rd <- floor(length(predicted)/6)
  }
  else
    breaks_rd <- NULL

  all <- data.frame(cbind(actual,unname(predicted)))
  cases_all <- data.frame(subset(all, all[,1]==1))
  control_all <- data.frame(subset(all, all[,1]==0))

  x_original <- format_values(cases_all, control_all, 0)
  x_scaled <- format_values(cases_all, control_all, 1)
  x_transformed <- format_values(cases_all, control_all, 2)

  switch(model_idx,
    "1"= {
      model <- build_hist_binning(x_scaled$formated_values[,1],x_scaled$formated_values[,2])
      model$inputtype <- 1
      model$model_idx <- model_idx
      return(list(hist_scaled=model))
    },
    "2"= {
      model <- build_hist_binning(x_transformed$formated_values[,1],x_transformed$formated_values[,2])
      model$inputtype <- 2
      model$model_idx <- model_idx
      return(list(hist_transformed=model))
    },
    "3"= {
      model <- build_BBQ(x_scaled$formated_values[,1],x_scaled$formated_values[,2])
      model$inputtype <- 1
      model$model_idx <- model_idx
      return(list(BBQ_scaled=model))
    },
    "4"= {
      model <- build_BBQ(x_transformed$formated_values[,1],x_transformed$formated_values[,2])
      model$inputtype <- 2
      model$model_idx <- model_idx
      return(list(BBQ_transformed=model))

    },
    "5"={
      model <- build_GUESS(x_original$formated_values[,1],x_original$formated_values[,2])
      model$inputtype <- 0
      model$model_idx <- model_idx
      return(list(GUESS=model))
    })}
