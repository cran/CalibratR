#' @title evaluate_discrimination
#' @description computes various discrimination error values, namely: sensitivity, specificity, accuracy, positive predictive value (ppv), negative predictive value (npv) and AUC
#' @param actual vector of observed class labels (0/1)
#' @param predicted vector of uncalibrated predictions
#' @param cutoff cut-off to be used for the computation of npv, ppv, sensitivity and specificity, Default: value that maximizes sensitivity and specificity (Youden-Index)
#' @return list object with the following components:
#' \item{sens}{sensitivity}
#' \item{spec}{specificity}
#' \item{acc}{accuracy}
#' \item{ppv}{positive predictive value}
#' \item{npv}{negative predictive value}
#' \item{cutoff}{cut-off that was used to compute the error values}
#' \item{auc}{AUC value}
#' @seealso
#'  \code{\link[pROC]{roc}}
#' @rdname evaluate_discrimination
#' @importFrom pROC roc
#'
evaluate_discrimination <- function(actual, predicted, cutoff=NULL){
###local functions###
  getAUC <- function(actual, predicted){
    if (length(unique(actual))!=2||max(unique(actual))!=1){ #actual hast to be 0 or 1
      warning("strange input")
    }
    nTarget <- length(which(actual==1)) #how many y==1
    nBackground <- length(which(actual!=1)) #how many y==0

    #Rank data
    R <- rank(predicted, ties.method = "average")

    #Calculate AUC using Wilcoxon Signed Rank Test
    AUC <- (sum(R[which(actual==1)])-(nTarget^2+nTarget)/2) / (nTarget*nBackground) #Ranksum
    AUC <- max (AUC, 1-AUC)
  }
  discriminate <- function(i, cutoff){
    #decides on i's 1/0 class membership by checking, if i is greater than the threshold value cutoff

    if (i>cutoff){
      class <- 1
    }
    else
      class <- 0

    return(class)
  }

  if(is.null(cutoff)){
    roc <- pROC::roc(actual, predicted)
    youden <- which.max(roc$sensitivities + roc$specificities-1) #calculate maximum of Youden Index
    cutoff <- roc$thresholds[youden]
  }
  else{
    youden <- cutoff
  }

  output_class <- sapply(predicted, discriminate, cutoff=cutoff)

  true_positives <- which(actual==1)
  true_negatives <- which(actual==0)

  #sensitivity, specificity
  sens <- sum(output_class[true_positives]==1)/length(true_positives)
  spec <- sum(output_class[true_negatives]==0)/length(true_negatives)
  false_positive <- sum(output_class[true_negatives]==1)/length(true_negatives)
  false_negative <- sum(output_class[true_positives]==0)/length(true_positives)
  ppv <- sum(output_class[true_positives]==1)/(sum(output_class[true_positives]==1)+sum(output_class[true_negatives]==1))
  npv <- sum(output_class[true_negatives]==0)/(sum(output_class[true_positives]==0)+sum(output_class[true_negatives]==0))

  all <- length(actual)

  #AUC
  auc <- getAUC(actual, predicted)

  #accuracy
  acc <- (sum(output_class[true_positives]==1)+sum(output_class[true_negatives]==0))/all

  error_list <- list(sens=sens, spec=spec, acc=acc, ppv=ppv, npv=npv, cutoff=cutoff, auc=auc)
  rounded_list <- lapply(error_list,FUN=round,3)

  return(rounded_list)
}

