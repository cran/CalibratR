#' @title format_values
#' @description returns formatted input.
#' If specified, the uncalibrated input is mapped to the [0;1] range using scaling (\code{\link{scale_me}}) or transforming (\code{\link{transform_me}})
#' @param cases instances from class 1
#' @param control instances from class 0
#' @param input single integer (0, 1 or 2). specify if the input should be formatted (=0), formatted and scaled (=1)
#' or formatted and transformed (=2)
#' @param min min value of the original data set, default=calculated on input
#' @param max max value of the original data set, default=calculated on input
#' @param mean mean value of the original data set, default=calculated on input
#' @return list object with the following components:
#' \item{formated_values}{formatted input. If \code{input} is set to 1 (2), the input is additionally scaled (transformed) using the
#' method \code{\link{scale_me}} (\code{\link{transform_me}})}
#' \item{min}{minimum value among all instances}
#' \item{max}{maximum value among all instances}
#' \item{mean}{mean value among all instances}
#' @rdname format_values

format_values <- function(cases, control, input, min=NULL, max=NULL, mean=NULL){

  simulation <- c(cases[,2], control[,2])
  simulation_real <- c(cases[,1], control[,1])

  #return min/max for scaling
  if (is.null(max) || is.null(min)|| is.null(mean)){
    min <- min(min(cases), min(control), na.rm=TRUE)
    max <- max(max(cases), max(control), na.rm=TRUE)
    mean <- mean(simulation, na.rm = TRUE)
  }

  if (input==0){
    output <- cbind(simulation_real, simulation)
  }
  else
    if (input==1){
    output <- cbind(simulation_real, scale_me(simulation, min=min, max=max))
    }
  else
    if (input==2){
      output <- cbind(simulation_real, transform_me(simulation, mean=mean))
    }

  return(list(formated_values=output, min=min, max=max, mean=mean))
}

#' @title transform_me
#' @description maps all instances in \code{x_unscaled} to the [0;1] range using the equation:
#' \cr  y=exp(x)/(1+exp(x))
#' @param x_unscaled vector of predictions
#' @param mean mean of \code{x}
#' @return transformed values of \code{x_unscaled}
#' @details values greater then exp(700)/ or smaller then exp(-700) are returned as "Inf". To avoid NaN values, these "Inf." values are turned into min(y) or max(y).
#' @rdname transform_me

transform_me <- function(x_unscaled, mean){

  #center first, subtract mean of x_unscaled from all x_unscaled values to center around 0
  x <- scale(x_unscaled, center=mean, scale=FALSE)[,]

  #transform x
  y <- exp(x)/(1+exp(x))

  for (i in 1:length(y)){
    if (is.nan(y[i]) && x[i]>0){
      y[i] <- max(y, na.rm=TRUE)
    }
    if (is.nan(y[i]) && x[i]<0){
      y[i] <- min(y, na.rm=TRUE)
    }
  }
  return(y)
}

#' @title scale_me
#' @description maps all instances in \code{x} to the [0;1] range using the equation:
#' \cr y = (x-min)/(max-min)
#' \cr If no values for min and max are given, they are calculated per default as min=min(x) and max=max(x)
#' @param x vector of predictions
#' @param min minimum of \code{x}, Default: NULL
#' @param max maximum of \code{x}, Default: NULL
#' @return scaled values of \code{x}
#' @details if \code{x} is greater (smaller) than \code{max} (\code{min}), its calibrated prediction is set to 1 (0) and warning is triggered.
#' @rdname scale_me

scale_me <- function(x, min=NULL, max=NULL){

  if (is.null(max) || is.null(min)){
    max <- max(x, na.rm=TRUE)
    min <- min(x, na.rm=TRUE)
  }

  y <- (x-min)/(max-min)

  if(any(y<0)){
    y[(y<0)] <- 0
    warning("A new instance exceeded the min value of the calibration training model
            and was set to 0 value")
  }

  if(any(y>1)){
    y[(y>1)] <- 1
    warning("A new instance exceeded the max value of the calibration training model
            and was set to 1 value")
  }
  return(y)
}
