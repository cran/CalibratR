#' @title predict_calibratR
#' @description maps the uncalibrated predictions \code{new} into calibrated predictions using the passed over \code{calibration models}
#' @author Johanna Schwarz
#' @param calibration_models list of trained calibration models that were constructed using the \code{\link{calibrate}} method.
#' The list components \code{calibration_models} from the \code{\link{calibrate}} output can be used directly.
#' @param new vector of new uncalibrated instances. Default: 100 scores from the minimum to the maximum of the original ML scores
#' @param nCores \code{nCores} how many cores should be used during parallelisation. Default: 4
#' @return list object with the following components:
#' \item{predictions}{a list containing the calibrated predictions for each calibration model}
#' \item{significance_test_set}{a list containing the percentage of \code{new} instances for which prediction estimates are statistically significant}
#' \item{pred_per_bin}{a list containing the number of instances in each bin for the binning models}
#' @details if no \code{new} value is given, the function will evaluate a sequence of numbers ranging from the minimum to the maximum of the original values in the training set
#' @examples
#'  ## Loading dataset in environment
#'  data(example)
#'  test_set <- example$test_set
#'  calibration_model <- example$calibration_model
#'
#'  ## Predict for test set
#' predictions <-  predict_calibratR(calibration_model$calibration_models, new=test_set, nCores = 2)
#'
#' @rdname predict_calibratR
#' @export
#' @importFrom parallel makeCluster stopCluster
#' @import foreach
#' @importFrom doParallel registerDoParallel

predict_calibratR <- function(calibration_models, new=NULL, nCores=4){


  min <- min(calibration_models$original_values$predicted)
  max <- max(calibration_models$original_values$predicted)
  mean <- mean(calibration_models$original_values$predicted)

  #default: if no seq is given, evaluate from min to max value of original input score
  if(is.null(new)){
    step_size <- (max-min)/100 #evaluate 100 scores
    new <- seq(min, max, step_size)
  }


  #calibrated predictions, inputtype 1=scaled, 2=transformed, 0=original
  NumberOfCluster <- nCores # how many jobs you want the computer to run at the same time
  cl <- parallel::makeCluster(NumberOfCluster) # use the above cluster # your parallel programming code code code stopCluster(cl) # close clusters
  doParallel::registerDoParallel(cl)
  `%dopar%` <- foreach::`%dopar%`
  i <- NULL


  predictions_calibrated <- foreach::foreach(i=seq(1, length(calibration_models$models),1), .packages = "CalibratR") %dopar% {

    pred <- predict_model(new, calibration_models$models[[i]], min, max, mean, calibration_models$models[[i]]$inputtype)
    return(pred)
  }
  parallel::stopCluster(cl)
  names(predictions_calibrated) <- names(calibration_models$models)

  #restructure predictions_calibrated
  predictions_calibrated[["hist_scaled"]] <- predictions_calibrated$hist_scaled$predictions
  predictions_calibrated[["hist_transformed"]] <- predictions_calibrated$hist_transformed$predictions
  predictions_calibrated[["BBQ_scaled_sel"]] <- predictions_calibrated$BBQ_scaled$BBQ_sel$predictions
  predictions_calibrated[["BBQ_scaled_avg"]] <- predictions_calibrated$BBQ_scaled$BBQ_avg$predictions
  predictions_calibrated$BBQ_scaled <- NULL
  predictions_calibrated[["BBQ_transformed_sel"]] <- predictions_calibrated$BBQ_transformed$BBQ_sel$predictions
  predictions_calibrated[["BBQ_transformed_avg"]] <- predictions_calibrated$BBQ_transformed$BBQ_avg$predictions
  predictions_calibrated$BBQ_transformed <- NULL
  predictions_calibrated[["GUESS_1"]] <- predictions_calibrated$GUESS$GUESS_1$predictions
  predictions_calibrated[["GUESS_2"]] <- predictions_calibrated$GUESS$GUESS_2$predictions
  predictions_calibrated$GUESS <- NULL

  #uncalibrated predictions
  predictions <- list()
  predictions[["original"]] <- new
  predictions[["scaled"]] <- scale_me(new, min, max)
  predictions[["transformed"]] <- transform_me(new, mean)

  predictions <- c(predictions, predictions_calibrated)

  return(predictions)
}

