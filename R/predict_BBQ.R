#' @title predict_BBQ
#' @description FUNCTION_DESCRIPTION
#' @param bbq output from the \code{\link{build_BBQ}} method
#' @param new vector of uncalibrated probabilities
#' @param option either 1 or 0; averaging=1, selecting=0
#' @return  a list object containing the following components:
#' \item{predictions}{contains a vector of calibrated predictions}
#' \item{pred_idx}{which option was used (averaging or selecting)}
#' \item{significance_test_set}{the percentage of \code{new} instances that was evaluated using significant prediction estimates}
#' \item{pred_per_bin}{number of instances \code{new} in each bin of the selected model}
#' @details Based on the paper (and matlab code) : "Obtaining Well Calibrated Probabilities Using Bayesian Binning" by Naeini, Cooper and Hauskrecht: ; https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4410090/
#' @rdname predict_BBQ


predict_BBQ <- function(bbq, new, option){
  ###local functions###
  getHistPr <- function(histModel, cutPoints, new){
    N <- length(new) #new elements to be predicted
    B <- length(histModel) #how many bins are in the model
    cutPoints <- c(0,cutPoints,1)
    res <- rep(0,N)

    for (i in 1:N){ #for each new element N
      x <- new[i]
      minIdx <- 1
      maxIdx <- B+1

      #in which bin does my element belong?
      while ((maxIdx - minIdx)>1){
        midIdx <- floor((minIdx+maxIdx)/2) #I start looking in the middle
        if(x>cutPoints[midIdx]){
          minIdx <- midIdx
        }
        else
          if(x < cutPoints[midIdx]){
            maxIdx <- midIdx
          }
        else{
          minIdx <- midIdx
          break
        }
      }
      idx <- minIdx
      res[i] <- histModel[[idx]]$P #assign class prob P for bin idx to new resultat for element i

      #handling odd cases, not really relevant according to paper?
      cnt <- 1
      k <- idx -1
      while (k>=1){
        if (histModel[[k]]$min==histModel[[idx]]$min && histModel[[k]]$max==histModel[[idx]]$max){
          res[i] <- res[i] + histModel[[k]]$P
          k <- k+1
          cnt <- cnt+1
        }
        else
          break
      }
      res[i] <- res[i]/cnt
    }
    return(res)
  }

  getMA <- function(BBQ_Model, x){ #get Model average
    N <- length(BBQ_Model) #how many models
    p <- rep(0,N)
    SV <- BBQ_Model[[1]]$SV #all the scores for all models

    for(i in 1:N){ #get the probs for the new prediction from all evaluated models
      p[i] <- getHistPr(BBQ_Model[[i]]$binNo, BBQ_Model[[i]]$cutPoints, x)
    }

    #output average p
    res <- (t(SV)%*%p)/sum(SV) #transpose and matrix multiplication
  }

  out <- rep(0, length(new))
  BBQ_Model <- bbq$prunedmodel

  if (option==1){#option for averaging
    for (i in 1:length(new)){
      out[i] <- getMA(BBQ_Model, new[i])
    }
  #percentage of significant predictions for test set if best model is used
  sign_test_set <- NULL
  new_bin <- NULL
  }

  if(option==0){#option for selection
    for (i in 1:length(new)){
      out[i] <- getHistPr(BBQ_Model[[1]]$binNo, BBQ_Model[[1]]$cutPoints, new[i])
    }

  #percentage of significant predictions for test set if best model is used
  significant_bins <- subset(bbq$binnning_scheme$bin_no, bbq$binnning_scheme$p_value<0.05)
  new_bin <- cut(new, c(0,BBQ_Model[[1]]$cutPoints,1),labels = seq(1,length(bbq$binnning_scheme$midpoint)),include.lowest = T)
  sign_test_set <- sum(table(new_bin)[significant_bins])/(sum(table(new_bin)))
  }


  return(list(predictions=out, pred_idx=option, significance_test_set=sign_test_set, pred_per_bin=table(new_bin)))
}
