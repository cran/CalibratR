#' @title statistics_calibratR
#' @description this method offers a variety of statistical evaluation methods for the output of the \code{\link{calibrate}} method.
#' All returned error values represent mean error values over the \code{n_seeds} times repeated 10-fold CV.
#' @author Johanna Schwarz
#' @param calibrate_object list that is returned from the \code{\link{calibrate}} function. The parameter \code{n_seeds} is available as a list component of the \code{calibrate_object}
#' @param t.test_partitions Performs a paired two sided t.test over the error values (ECE, CLE1, CLE0, MCE, AUC, sensitivity and specificity) from the
#' random partition splits comparing a possible significant difference in mean among the calibration models. All models and the original, scaled and transformed values are tested against each other.
#' The p_value and the effect size of the t.test are returned to the user. Can only be performed, if the \code{calibrate_object} contains a \code{summary_CV} list object, else, an error is returned.  Default: TRUE
#' @param significance_models returns important characteristics of the implemented calibration models, Default: TRUE
#' @return  An object of class list, with the following components:
#' \item{mean_calibration}{mean of calibration error values (ECE_equal_width, MCE_equal_width, ECE_equal_freq, MCE_equal_freq, RMSE, Class 1 CLE, Class 0 CLE, Brier Score, Class 1 Brier Score, Class 0 Brier Score) over \code{n_seeds} times repeated 10-fold CV.
#' ECE and MCE are computed once using equal-width and once using equal-frequency binning for the construction of the underlying binning scheme.
#'   Only returned, if \code{calibrate_object} contains a summary_CV list object.}
#' \item{standard_deviation}{standard deviation of calibration error values over \code{n_seeds} times repeated 10-fold CV. Only returned, if \code{calibrate_object} contains a summary_CV list object.}
#' \item{var_coeff_calibration}{variation coefficient of calibration error values over \code{n_seeds} times repeated 10-fold CV. Only returned, if \code{calibrate_object} contains a summary_CV list object.}
#' \item{mean_discrimination}{mean of discrimination error (sensitivity, specificity, AUC, positive predictive value, negative predictive value, accuracy) values over \code{n_seeds} times repeated 10-fold CV. The "cut-off" is
#' the cut-off value that maximizes sensitivity and specificity. Only returned, if \code{calibrate_object} contains a summary_CV list object.}
#' \item{sd_discrimination}{standard deviation of discrimination error values over \code{n_seeds} times repeated 10-fold CV. Only returned, if \code{calibrate_object} contains a summary_CV list object.}
#' \item{var_coeff_discrimination}{variation coefficient of discrimination error values over \code{n_seeds} times repeated 10-fold CV. Only returned, if \code{calibrate_object} contains a summary_CV list object.}
#' \item{t.test_calibration}{=list(p_value=t.test.calibration, effect_size=effect_size_calibration), only returned if t.test=TRUE}
#' \item{t.test_discrimination}{=list(p_value=t.test.discrimination, effect_size=effect_size_discrimination), only returned if t.test=TRUE}
#' \item{significance_models}{only returned if significance_models=TRUE}
#' \item{n_seeds}{number of random data set partitions into training and test set for \code{folds}-times CV}
#' \item{original_values}{list object that consists of the \code{actual} and \code{predicted} values of the original scores}
#' @details DETAILS
#' @examples
#'  ## Loading dataset in environment
#'  data(example)
#'  calibration_model <- example$calibration_model
#'
#'  statistics <- statistics_calibratR(calibration_model)
#' @seealso
#'  \code{\link[stats]{t.test}},\code{\link[stats]{friedman.test}}
#' @rdname statistics_calibratR
#' @export
#' @importFrom stats t.test sd


statistics_calibratR <- function(calibrate_object, t.test_partitions=TRUE, significance_models=TRUE){

  if(!is.null(calibrate_object$summary_CV$models$calibrated)){

    ##data preparation
    means_calibration <- data.frame()
    sd_calibration <- data.frame()
    var_coeff_calibration <- data.frame()
    compare_ece <- data.frame()
    compare_mce <- data.frame()
    compare_auc <- data.frame()
    compare_rmse <- data.frame()
    compare_cle1 <- data.frame()
    compare_cle0 <- data.frame()
    compare_sens <- data.frame()
    compare_spec <- data.frame()

    for(i in calibrate_object$summary_CV$error_models$calibration){
      compare_ece <- rbind(compare_ece,i$ECE_equal_width)
      compare_cle1 <- rbind(compare_cle1,i$CLE_class_1)
      compare_cle0 <- rbind(compare_cle0,i$CLE_class_0)
      compare_mce <- rbind(compare_mce, i$MCE_equal_width)
      compare_rmse <- rbind(compare_rmse, i$RMSE)
      means_calibration <- rbind(means_calibration, apply(i,2, mean))
      sd_calibration <- rbind(sd_calibration, apply(i,2, sd))
    }

    model_names <- names(calibrate_object$summary_CV$error_models$calibration)
    n_runs <- seq(1,calibrate_object$n_seeds,1)
    names_calibration_errors <- names(calibrate_object$summary_CV$error_models$calibration[[1]])
    names_discrimination_errors <- names(calibrate_object$summary_CV$error_models$discrimination[[1]])

    rownames(compare_ece) <- model_names
    colnames(compare_ece) <- n_runs
    rownames(compare_rmse) <- model_names
    colnames(compare_rmse) <- n_runs
    rownames(compare_cle1) <- model_names
    colnames(compare_cle1) <- n_runs
    rownames(compare_cle0) <- model_names
    colnames(compare_cle0) <- n_runs
    rownames(compare_mce) <- model_names
    colnames(compare_mce) <- n_runs
    rownames(means_calibration) <- model_names
    colnames(means_calibration) <- names_calibration_errors
    rownames(sd_calibration) <- model_names
    colnames(sd_calibration) <- names_calibration_errors
    var_coeff_calibration <- sd_calibration/means_calibration

    means_discrimination <- data.frame()
    sd_discrimination <- data.frame()
    var_coeff_discrimination <- data.frame()

    for(i in calibrate_object$summary_CV$error_models$discrimination){
      means_discrimination <- rbind(means_discrimination, apply(i,2, mean))
      sd_discrimination <- rbind(sd_discrimination, apply(i,2, sd))
      compare_auc <- rbind(compare_auc,i$auc)
      compare_sens <- rbind(compare_sens,i$sens)
      compare_spec <-rbind(compare_spec,i$spec)
    }

    rownames(compare_auc) <- model_names
    colnames(compare_auc) <- n_runs
    rownames(compare_sens) <- model_names
    colnames(compare_sens) <- n_runs
    rownames(compare_spec) <- model_names
    colnames(compare_spec) <- n_runs
    rownames(means_discrimination) <- model_names
    colnames(means_discrimination) <- names_discrimination_errors
    rownames(sd_discrimination) <- model_names
    colnames(sd_discrimination) <- names_discrimination_errors
    var_coeff_discrimination <- sd_discrimination/means_discrimination
    all_calibration_errors <- list(ece=compare_ece, cle0=compare_cle0, cle1=compare_cle1,
                                   mce=compare_mce, rmse=compare_rmse)
    all_discrimination_errors <- list(auc=compare_auc, sens=compare_sens, spec=compare_spec)


    ## perform paired t.test for all models
    if(t.test_partitions){

    t.test_partitions_cal <- list()
    t.test_partitions_dis <- list()
    t.test.calibration <- list()
    t.test.discrimination <- list()
    effect_size_calibration <- list()
    effect_size_discrimination <- list()

    z <- 1
    a <- 1

    for (i in all_calibration_errors){
      t.test.all <- c()
      effect_size_all <- c()
      for (w in seq(1, nrow(i),1)){
        t.test <- c()
        effect_size <-c()
        mean <- c()
        sd <- c()
        for (y in seq(1, nrow(i),1)){
          #if observation for i[y,] are unique, do not perform t test or else it will crash
          if(length(unique(as.numeric(i[y,])))==1){
            t.test <- c(t.test, NA)
            effect_size <- c(effect_size, NA)
          }
          else{
            t.test_result <- stats::t.test(as.numeric(i[w,]), as.numeric(i[y,]), paired=TRUE)
            t.test <- c(t.test, round(t.test_result$p.value, 4))
            effect_size <- c(effect_size, round(t.test_result$estimate,4))
          }
          mean <- c(mean, mean(as.numeric(i[y,])))
          sd <- c(sd, sd(as.numeric(i[y,])))
        }
        t.test.all <- cbind(t.test.all, t.test)
        effect_size_all <- cbind(effect_size_all, effect_size)
        t.test_partitions_cal[[z]] <- cbind(i, mean=mean, sd=sd, rank_mean=rank(mean))
        z <- z+1
      }
      row.names(t.test.all) <- model_names
      colnames(t.test.all) <- model_names
      row.names(effect_size_all) <- model_names
      colnames(effect_size_all) <- model_names
      t.test.calibration[[a]] <- cbind(t.test.all, mean=round(mean,4), rank_mean=rank(mean))
      effect_size_calibration[[a]] <- effect_size_all
      a <- a+1
    }
    names(t.test.calibration) <- c("ece", "cle0", "cle1", "mce", "rmse")
    names(t.test_partitions_cal) <- c("ece", "cle0", "cle1", "mce", "rmse")
    names(effect_size_calibration) <- c("ece", "cle0", "cle1", "mce", "rmse")

    z <- 1
    a <- 1

    for (i in all_discrimination_errors){
      t.test.all <- c()
      effect_size_all <- c()

      for (w in seq(1, nrow(i),1)){
        t.test <- c()
        effect_size <- c()
        mean <- c()
        sd <- c()
        for (y in seq(1, nrow(i),1)){
          #if observation for i[y,] are unique, do not perform t test or else it will crash
          if(length(unique(as.numeric(i[y,])))==1){
            t.test <- c(t.test, NA)
            effect_size <- c(effect_size, NA)
          }
          else{
            t.test_result <- stats::t.test(as.numeric(i[w,]), as.numeric(i[y,]), paired=TRUE)
            t.test <- c(t.test, round(t.test_result$p.value, 4))
            effect_size <- c(effect_size, round(t.test_result$estimate,4))
          }
          mean <- c(mean, mean(as.numeric(i[y,])))
          sd <- c(sd, sd(as.numeric(i[y,])))
        }
        t.test.all <- cbind(t.test.all, t.test)
        effect_size_all <- cbind(effect_size_all, effect_size)
        t.test_partitions_dis[[z]] <- cbind(i, mean=mean, sd=sd, rank_mean=rank(-mean))
        z <- z+1
      }
      row.names(t.test.all) <- model_names
      colnames(t.test.all) <- model_names
      row.names(effect_size_all) <- model_names
      colnames(effect_size_all) <- model_names
      t.test.discrimination[[a]] <- cbind(t.test.all, mean=round(mean,4), rank_mean=rank(-mean))
      effect_size_discrimination[[a]] <- effect_size_all
      a <- a+1
    }
    names(t.test.discrimination) <- c("auc", "sens","spec")
    names(t.test_partitions_dis) <- c("auc", "sens","spec")
    names(effect_size_discrimination) <- c("auc", "sens","spec")
  }
    else {
      t.test_partitions_cal <- NULL
      t.test_partitions_dis <- NULL
      t.test.calibration <- NULL
      t.test.discrimination <- NULL
      effect_size_calibration <- NULL
      effect_size_discrimination <- NULL
    }
      }
  else{
    if(t.test_partitions==TRUE){
      warning("No error values from repeated CVs are available in the trained calibration models. No t-test can be performed.
                     Please make sure that the calibrate_object containes a summary_CV list object.")
    }
    t.test_partitions_cal <- NULL
    t.test_partitions_dis <- NULL
    t.test.calibration <- NULL
    t.test.discrimination <- NULL
    effect_size_calibration <- NULL
    effect_size_discrimination <- NULL
    means_calibration <- NULL
    sd_calibration <- NULL
    means_discrimination <- NULL
    sd_discrimination <- NULL
    var_coeff_calibration <- NULL
    var_coeff_discrimination <- NULL

  }

    if(significance_models){
    sign_model <- list()
    sign_model[["hist_scaled"]] <- list()
    sign_model[["hist_scaled"]] <- c(calibrate_object$calibration_models$models_final$hist_scaled$calibration_points,
                                     calibrate_object$calibration_models$models_final$hist_scaled$calibration_points_number,
                                     calibrate_object$calibration_models$models_final$hist_scaled$calibration_range)
    sign_model[["hist_transformed"]] <- list()
    sign_model[["hist_transformed"]] <- c(calibrate_object$calibration_models$models_final$hist_transformed$calibration_points,
                                          calibrate_object$calibration_models$models_final$hist_transformed$calibration_points_number,
                                          calibrate_object$calibration_models$models_final$hist_transformed$calibration_range)
    sign_model[["BBQ_scaled"]] <- list()
    sign_model[["BBQ_scaled"]] <- c(calibrate_object$calibration_models$models_final$BBQ_scaled$calibration_points,
                                    calibrate_object$calibration_models$models_final$BBQ_scaled$calibration_points_number,
                                    calibrate_object$calibration_models$models_final$BBQ_scaled$calibration_range)
    sign_model[["BBQ_transformed"]] <- list()
    sign_model[["BBQ_transformed"]] <- c(calibrate_object$calibration_models$models_final$BBQ_transformed$calibration_points,
                                         calibrate_object$calibration_models$models_final$BBQ_transformed$calibration_points_number,
                                         calibrate_object$calibration_models$models_final$BBQ_transformed$calibration_range)
    sign_model[["GUESS"]] <- list()
    sign_model[["GUESS"]] <- list(crit_boundaries=calibrate_object$calibration_models$models_final$GUESS$t_crit,
                               sign_train_set=calibrate_object$calibration_models$models_final$GUESS$sign_train_set)
  }
    else {
      sign_model <- NULL
    }
  return(list(mean_calibration=means_calibration, sd_calibration=sd_calibration, var_coeff_calibration=var_coeff_calibration,
              mean_discrimination=means_discrimination, sd_discrimination=sd_discrimination, var_coeff_discrimination=var_coeff_discrimination,
              t.test_calibration=list(p_value=t.test.calibration, effect_size=effect_size_calibration),
              t.test_discrimination=list(p_value=t.test.discrimination, effect_size=effect_size_discrimination),
              significance_models=sign_model,
              n_seeds=calibrate_object$n_seeds,
              original_values=calibrate_object$calibration_models$original_values))

}
