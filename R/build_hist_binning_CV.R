#' @title hist_binning_CV
#' @description trains and evaluates the histogram binning calibration model repeated \code{folds}-Cross-Validation (CV).
#' The \code{predicted} values are partitioned into n subsets. A histogram binning model is constructed on (n-1) subsets; the remaining set is used
#' for testing the model. All test set predictions are merged and used to compute error metrics for the model.
#' @param actual vector of observed class labels (0/1)
#' @param predicted vector of uncalibrated predictions
#' @param n_bins number of bins used in the histogram binning scheme, Default: 15
#' @param n_folds number of folds in the cross-validation, Default: 10
#' @param seed random seed to alternate the split of data set partitions
#' @param input specify if the input was scaled or transformed, scaled=1, transformed=2
#' @return list object containing the following components:
#' \item{error}{list object that summarizes discrimination and calibration errors obtained during the CV}
#' \item{type}{"hist"}
#' \item{probs_CV}{vector of calibrated predictions that was used during the CV}
#' \item{actual_CV}{respective vector of true values (0 or 1) that was used during the CV}
#' @rdname hist_binning_CV


hist_binning_CV <- function(actual, predicted, n_bins=15, n_folds=10, seed, input){
  set.seed(seed)

  x <- data.frame(cbind(actual, predicted))
  x_cases <- subset(x, x[,1]==1)
  x_controls <- subset(x, x[,1]==0)
  fold_cases <- sample(cut(seq(1,nrow(x_cases)),breaks=n_folds,label=FALSE))
  fold_controls <- sample(cut(seq(1,nrow(x_controls)),breaks=n_folds,label=FALSE))

  y_cal <- list()
  y_dis <- list()
  list_calibrated_probs <- c()
  list_actual <- c()

  error_fold <- c()
  hist_models <- list()
  hist_models_rd <- list()

  for(i in 1:n_folds){
    trainIndexes_cases <- which(fold_cases!=i, arr.ind = TRUE)
    trainIndexes_controls <- which(fold_controls!=i,arr.ind=TRUE)
    trainData <- rbind(x_cases[trainIndexes_cases, ], x_controls[trainIndexes_controls,])
    x_train <- format_values(x_cases[trainIndexes_cases, ],  x_controls[trainIndexes_controls,], input=input)

    testIndexes_cases <- which(fold_cases==i,arr.ind=TRUE)
    testIndexes_controls <- which(fold_controls==i,arr.ind=TRUE)
    testData <- rbind(x_cases[testIndexes_cases, ], x_controls[testIndexes_controls,])
    x_test <- format_values(x_cases[testIndexes_cases, ],  x_controls[testIndexes_controls,], input=input,
                            min=x_train$min, max=x_train$max, mean=x_train$mean)

    hist <- build_hist_binning(x_train$formated_values[,1], x_train$formated_values[,2], n_bins)
    calibrated_probs <- predict_hist_binning(hist, x_test$formated_values[,2])
    list_calibrated_probs <- c(list_calibrated_probs, calibrated_probs$predictions)
    list_actual <- c(list_actual,x_test$formated_values[,1])
    hist_models[[i]] <- hist

  }

  y <- reliability_diagramm(list_actual, list_calibrated_probs)
  y_cal <- y$calibration_error
  y_dis <- y$discrimination_error

  error_summary_CV <- list(calibration_error=y_cal, discrimination_error=y_dis,
                           mean_pred_per_bin=y$mean_pred_per_bin, accuracy_per_bin=y$accuracy_per_bin,
                           sign=y$sign)

  return(list(error=error_summary_CV, type="hist", probs_CV=list_calibrated_probs, actual_CV=list_actual))
}
