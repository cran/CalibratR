#' @title predict_model
#' @description calibrates the uncalibrated predictions \code{new} using \code{calibration_model}.
#' @param new vector of uncalibrated predictions
#' @param calibration_model calibration model to be used for the calibration. Can be the output of \code{\link{build_BBQ}},\code{\link{build_hist_binning}} or \code{\link{build_GUESS}}.
#' @param min minimum value of the original data set
#' @param max maximum value of the original data set
#' @param mean mean value of the original data set
#' @param inputtype specify if the model was build on original (=0), scaled(=1) or transformed (=2) data
#' @return vector of calibrated predictions
#' @rdname predict_model

predict_model <- function(new, calibration_model, min, max, mean, inputtype){
###locale function###
  prepare_input <- function(new, min, max, mean, inputtype){

    if (inputtype==0){ #model uses original scores
      output <- new
    }
    else if (inputtype==1){ #model uses scaled scores
      output <- scale_me(new, min, max)
    }
    else if (inputtype==2){ #model uses transformed scores
      output <- transform_me(new, mean)
    }

    return(output=output)
  }
  predict <- switch(calibration_model$type,
         "hist"= predict_hist_binning,
         "BBQ"= predict_BBQ,
         "GUESS"= predict_GUESS
  )

  new <- prepare_input(new, min, max, mean, inputtype)

  if(calibration_model$type=="BBQ"){
    x_sel <- predict(calibration_model, new, 0)
    x_avg <- predict(calibration_model, new, 1)
    return(list(BBQ_sel=x_sel,
                BBQ_avg=x_avg))
  }

  else if (calibration_model$type=="hist"){
    x <- predict(calibration_model, new)
    return(case=x)
  }

  else if(calibration_model$type=="GUESS"){
    x_1 <- predict(calibration_model, new, 1)
    x_2 <- predict(calibration_model, new, 2)
    return(list(GUESS_1=x_1,
                GUESS_2=x_2))
  }
}
