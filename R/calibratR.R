
#' @title calibrate
#' @description Builds selected calibration models on the supplied trainings values \code{actual} and \code{predicted} and returns them
#' to the user. New test instances can be calibrated using the \code{\link{predict_calibratR}} function.
#' Returns cross-validated calibration and discrimination error values for the models if \code{evaluate_CV_error} is set to TRUE. Repeated cross-Validation can be time-consuming.
#' @author Johanna Schwarz
#' @param actual vector of observed class labels (0/1)
#' @param predicted vector of uncalibrated predictions
#' @param model_idx which calibration models should be implemented, 1=hist_scaled, 2=hist_transformed, 3=BBQ_scaled, 4=BBQ_transformed, 5=GUESS, Default: c(1, 2, 3, 4, 5)
#' @param evaluate_no_CV_error computes internal errors for calibration models that were trained on all available \code{actual}/\code{predicted} tuples. Testing is performed with the same set. Be careful to interpret those error values, as they are not cross-validated. Default: TRUE
#' @param evaluate_CV_error computes cross-validation error. \code{folds} times cross validation is repeated \code{n_seeds} times with changing seeds. The trained models and the their calibration and discrimination errors are returned.
#' Evaluation of CV errors can take some time to compute, depending on the number of repetitions specified in \code{n_seeds}, Default: TRUE
#' @param folds number of folds in the cross-validation of the calibration model. If \code{folds} is set to 1, no CV is performed and \code{summary_CV} can be calculated. Default: 10
#' @param n_seeds \code{n_seeds} determines how often random data set partition is repeated with varying seed. If \code{folds} is 1, \code{n_seeds} should be set to 1, too. Default: 30
#' @param nCores \code{nCores} how many cores should be used during parallelisation. Default: 4
#' @return A list object with the following components:
#' \item{calibration_models}{a list of all trained calibration models, which can be used in the \code{\link{predict_calibratR}} method.}
#' \item{summary_CV}{a list containing information on the CV errors of the implemented models}
#' \item{summary_no_CV}{a list containing information on the internal errors of the implemented models}
#' \item{predictions}{calibrated predictions for the original \code{predicted} values}
#' \item{n_seeds}{number of random data set partitions into training and test set for \code{folds}-times CV}
#' @details parallised execution of random data set splits for the Cross-Validation procedure over \code{n_seeds}
#' @examples
#'  ## Loading dataset in environment
#'  data(example)
#'  actual <- example$actual
#'  predicted <- example$predicted
#'
#'  ## Create calibration models
#'  calibration_model <- calibrate(actual, predicted,
#'                               model_idx = c(1,2),
#'                               FALSE, FALSE, folds = 10, n_seeds = 1, nCores = 2)
#' @rdname calibrate
#' @export
#' @importFrom parallel makeCluster stopCluster
#' @import foreach
#' @importFrom doParallel registerDoParallel

calibrate <- function(actual, predicted,
                      model_idx=c(1,2,3,4,5),
                      evaluate_no_CV_error=TRUE,
                      evaluate_CV_error=TRUE,
                      folds=10,
                      n_seeds=30,
                      nCores = 4
                      ){

  set.seed(123)

  if (length(actual) != length(predicted)){
    stop("Please make sure, that the parameters actual and predicted are of the same length.")
  }

  if (any((unique(actual)!=1) & (unique(actual)!=0))){
    stop("The parameter actual contains values other than 1 or 0. Please code your class labels accordingly.")
  }

  if(evaluate_CV_error==FALSE & (!is.null(folds)|| !is.null(n_seeds))){
    warning("No Cross-Validation is performed, but parameters folds or n_seeds are specified. If you want to perform CV, please set evaluate_CV_error TRUE.")
  }
  predicted <- unname(predicted)

  #original values
  original_values <- list(actual=actual, predicted=predicted)

  #build selected models on all data and predict for all data (=no CV)
  models_final <- list()
  cal_models_final <- list()

  for (i in model_idx){
    models_final <- c(models_final, calibrate_me(actual=actual, predicted=predicted, model_idx=i))
  }
  cal_models_final <- list(original_values=original_values, models_final=models_final)
  predictions <- predict_calibratR(cal_models_final, predicted, nCores)

  #performs x-fold CV and returns error values
  if(evaluate_CV_error){

    error <- c()
    t0_error <- list()
    t0_error[["calibration"]] <- list()
    t0_error[["discrimination"]] <- list()
    y <- 1

   #uncalibrated
    #parallize the foreach loop
    NumberOfCluster <- nCores # how many jobs you want the computer to run at the same time
    cl <- parallel::makeCluster(NumberOfCluster) # use the above cluster # your parallel programming code code code stopCluster(cl) # close clusters
    doParallel::registerDoParallel(cl)
    `%dopar%` <- foreach::`%dopar%`
    comb <- function(x, ...) {
      lapply(seq_along(x),
             function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
    }

    error <- foreach::foreach(i=seq(1,n_seeds,1), .packages = "CalibratR", .combine='comb',
                                     .multicombine=TRUE, .init=list(list(), list(), list()),
                                     .final = function(x) setNames(x, c("original", "scaled", "transformed"))) %dopar% {
      original <-  uncalibrated_CV(actual, predicted, n_folds=folds, seed=i, input=0)
      scaled <- uncalibrated_CV(actual, predicted, n_folds=folds, seed=i, input=1)
      transformed <- uncalibrated_CV(actual, predicted, n_folds=folds, seed=i, input=2)
      return(list(original=original, scaled=scaled, transformed=transformed))
      }
    parallel::stopCluster(cl)

    ##uncalibrated predictions
    for (model in error){
      calibration_df <- data.frame()
      discrimination_df <- data.frame()

      for(i in seq(1,length(model),1)){
        calibration_df <- rbind(calibration_df,unlist(model[[i]]$error$calibration_error))
        discrimination_df <- rbind(discrimination_df,unlist(model[[i]]$error$discrimination_error))
      }

      colnames(discrimination_df) <- names(model[[1]]$error$discrimination_error)
      colnames(calibration_df) <- names(model[[1]]$error$calibration_error)

      t0_error$calibration[[names(error)[y]]] <- calibration_df
      t0_error$discrimination[[names(error)[y]]] <- discrimination_df

      y <- y+1
    }

    #build calibration models
    y <- 1
    error_calibrated <- calibrate_me_CV_errors(actual, predicted, model_idx, folds, n_seeds,nCores)

    for(model in error_calibrated){
      if(!length(model)==0){

        calibration_df <- data.frame()
        discrimination_df <- data.frame()

        for(i in seq(1,length(model),1)){
          calibration_df <- rbind(calibration_df,unlist(model[[i]]$error$calibration_error))
          discrimination_df <- rbind(discrimination_df,unlist(model[[i]]$error$discrimination_error))
        }

        colnames(discrimination_df) <- names(model[[1]]$error$discrimination_error)
        colnames(calibration_df) <- names(model[[1]]$error$calibration_error)

        t0_error$calibration[[names(error_calibrated)[y]]] <- calibration_df
        t0_error$discrimination[[names(error_calibrated)[y]]] <- discrimination_df

        y <- y+1
      }
        else{
          y <- y+1
        }
    }}

  else{
    error <- NULL
    error_calibrated <- NULL
    t0_error <- NULL
  }

  #calculates error values on training set
  if(evaluate_no_CV_error){
    training_error <- c()
    y <- 1
    error_values_no_CV <- list()
    error_values_no_CV[["calibration"]] <- list()
    error_values_no_CV[["discrimination"]] <- list()

    for (i in predictions){
      training_error <- c(training_error, list(reliability_diagramm(actual, i)))
    }
    names(training_error) <- names(predictions)

    for (i in training_error){
      error_values_no_CV$calibration[[names(training_error)[y]]] <- unlist(i$calibration_error)
      error_values_no_CV$discrimination[[names(training_error)[y]]] <- unlist(i$discrimination_error)
      y <- y+1
    }

    df_calibration_no_CV <- t(data.frame(error_values_no_CV$calibration))
    df_discrimination_no_CV <- t(data.frame(error_values_no_CV$discrimination))
  }
  else{
    df_calibration_no_CV <- NULL
    df_discrimination_no_CV <- NULL
    training_error <- NULL
  }


  res <- list(calibration_models=cal_models_final,
           summary_CV=list(models=list(uncalibrated=error, calibrated=error_calibrated),error_models=t0_error,
                           folds=folds),
           summary_no_CV=list(calibration_error=df_calibration_no_CV,
                              discrimination_error=df_discrimination_no_CV,
                              list_errors=training_error),
           predictions=predictions,
           n_seeds=n_seeds)

  return(res)
}
