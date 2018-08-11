
#' @title calibrate_me_CV_errors
#' @description trains and evaluates calibration models using \code{n_seeds}-times repeated \code{folds}-Cross-Validation (CV).\code{model_idx} specifies which models should be trained.
#' \cr Model training and evaluation is repeated \code{n_seeds}-times with a different training/test set partition scheme for the CV each time.
#' @param actual vector of observed class labels (0/1)
#' @param predicted vector of uncalibrated predictions
#' @param model_idx which calibration models should be implemented, 1=hist_scaled, 2=hist_transformed, 3=BBQ_scaled, 4=BBQ_transformed, 5=GUESS
#' @param folds number of folds in the cross-validation, Default: 10
#' @param n_seeds \code{n_seeds} determines how often random data set partition is repeated with varying seed
#' @param nCores \code{nCores} how many cores should be used during parallelisation. Default: 4
#' @return returns all trained calibration models that were built during the \code{n_seeds}-times repeated \code{folds}-CV.
#' \cr Error values for each of the \code{n_seeds} CV runs are given.
#' @details parallised execution over \code{n_seeds}
#' @rdname calibrate_me_CV_errors
#' @importFrom parallel makeCluster stopCluster
#' @import foreach
#' @importFrom stats setNames
#' @importFrom doParallel registerDoParallel


calibrate_me_CV_errors <- function(actual, predicted, model_idx, folds=10, n_seeds, nCores){

  #parallize the foreach loop
  NumberOfCluster <- nCores # how many jobs you want the computer to run at the same time
  cl <- parallel::makeCluster(NumberOfCluster) # Make clusters registerDoSNOW(cl) # use the above cluster # your parallel programming code code code stopCluster(cl) # close clusters
  doParallel::registerDoParallel(cl)
  `%dopar%` <- foreach::`%dopar%`

  #how many list() do I expect in my output
  idx <- 0
  names_model <- c()
  if(any(model_idx==1)){
    idx <- idx +1
    names_model <- c(names_model,"hist_scaled")
  }
  if(any(model_idx==2)){
    idx <- idx +1
    names_model <- c(names_model,"hist_transformed")
  }
  if(any(model_idx==3)){
    idx <- idx +2
    names_model <- c(names_model,"BBQ_scaled_sel", "BBQ_scaled_avg")
  }
  if(any(model_idx==4)){
    idx <- idx +2
    names_model <- c(names_model,"BBQ_transformed_sel", "BBQ_transformed_avg")
  }
  if(any(model_idx==5)){
    idx <- idx +2
    names_model <- c(names_model,"GUESS_1", "GUESS_2")
  }

  comb <- function(x, ...) {
    lapply(seq_along(x),
           function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
  }

  n <- length(seq(1,n_seeds,1))
  i <- NULL
  hist_scaled <- NULL
  hist_transformed <- NULL
  BBQ_scaled_sel <- NULL
  BBQ_scaled_avg <- NULL
  BBQ_transformed_sel <- NULL
  BBQ_transformed_avg <- NULL
  GUESS_1 <- NULL
  GUESS_2 <- NULL

  parallized_results <- foreach::foreach(i=seq(1,n_seeds,1), .packages = "CalibratR", .combine='comb',
                                         .multicombine=TRUE, .init=rep(list(list()), idx),
                                         .final = function(x) setNames(x, names_model)) %dopar% {
    if(any(model_idx==1)){
      hist_scaled <- hist_binning_CV(actual, predicted, n_folds=folds, seed=i, input=1)
    }
    if(any(model_idx==2)){
      hist_transformed <- hist_binning_CV(actual, predicted, n_folds=folds, seed=i, input=2)
    }
    if(any(model_idx==3)){
      BBQ_scaled_sel <- BBQ_CV(actual, predicted,0, n_folds=folds, seed=i, input=1)
      BBQ_scaled_avg <- BBQ_CV(actual, predicted,1, n_folds=folds, seed=i, input=1)
    }
    if(any(model_idx==4)){
      BBQ_transformed_sel <- BBQ_CV(actual, predicted,0, n_folds=folds, seed=i, input=2)
      BBQ_transformed_avg <- BBQ_CV(actual, predicted,1, n_folds=folds, seed=i, input=2)
    }
    if(any(model_idx==5)){
      GUESS_1 <- GUESS_CV(actual, predicted, n_folds=folds,1, seed=i, input=0)
      GUESS_2 <- GUESS_CV(actual, predicted, n_folds=folds,2, seed=i, input=0)
    }

    list(hist_scaled=hist_scaled, hist_transformed=hist_transformed, BBQ_scaled_sel=BBQ_scaled_sel, BBQ_scaled_avg=BBQ_scaled_avg,
                    BBQ_transformed_sel=BBQ_transformed_sel, BBQ_transformed_avg=BBQ_transformed_avg, GUESS_1=GUESS_1, GUESS_2=GUESS_2)
    }
  parallel::stopCluster(cl)

  return(error=parallized_results)
}
