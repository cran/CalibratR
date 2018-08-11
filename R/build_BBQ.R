#' @title build_BBQ
#' @description This method builds a BBQ calibration model using the trainings set provided.
#' @param actual vector of observed class labels (0/1)
#' @param predicted vector of uncalibrated predictions
#' @return returns the BBQ model which includes models for all evaluated binning schemes; the prunedmodel contains only a selection of BBQ models with the best Bayesian score
#' @details Based on the paper (and matlab code) : "Obtaining Well Calibrated Probabilities Using Bayesian Binning" by Naeini, Cooper and Hauskrecht: ; https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4410090/
#' @rdname build_BBQ


build_BBQ <- function(actual, predicted){
  ####local functions###
  initlnfact_local <- function(n){
    lnfact <- rep(0,n+1)
    for (w in 2:(n+1)){
      lnfact[w] <- lnfact[w-1] + log(w-1)
    }
    return(lnfact)
  }

  get_BDeu_Score2_local <- function(opt){
    histModel <- opt$histModel
    B <- length(histModel)
    N_0 <- 2*B #default in paper
    C <- N_0/B # =2

    score <- B*lgamma(C)

    for (j in 1:B){
      nj <- histModel[[j]]$n #number of elements in bin B
      nj0 <- histModel[[j]]$n0 #number of y==0 elements in bin B
      nj1 <- histModel[[j]]$n1 #number of y==1 elements in bin B
      pj <- (histModel[[j]]$min + histModel[[j]]$max)/2 #midpoint of bin B, naive estimate of probability
      pj <- min(pj,1-5*10^-3)
      pj <- max(pj, 5*10^-3)
      score <- score + (lgamma(nj1+C*pj)) + (lgamma(nj0+C*(1-pj))) - (lgamma(nj+C)) - (lgamma(C*pj)) - (lgamma(C*(1-pj)))#log(marginal LL)test score(Pr?fgr??e)
    }

    score <- -2*score
    return (score)
  }

  buildFuncOpt_local <- function(opt, histModel, cutIdx, cutPoints, logLikelihood){
    N <- length(opt$PTR) #number of predicted values in the whole histogram
    K <- length(histModel) #how many bins where evaluated

    funcOpt <- list(histModel=histModel, cutIdx=cutIdx, cutPoints=cutPoints, logLikelihood=logLikelihood, K=K, N=N, PTR=opt$PTR, lnfact=opt$
                      lnfact, N_0=opt$N_0)
    return(funcOpt)
  }

  hist_calibration_freq_local <- function(predicted, actual, b){

    #get the log of the likelihood P(D|M) for the observed data distribution in the bin, given theta as P
    #P is circa the prevalence of positives in the bin, but is smoothed and also depends by midpoint of the bin and mean predicted value for a positive case

    N <- length(actual)
    logLikelihood <- 0
    cutIdx <- c() #b==1 in this case -> no cutIdx
    cutPoints <- c()
    histModel_all <- list()

    #smoothing function for p
    if (b==1){ #if binning model contains only 1 bin
      min <- 0
      max <- 1
      m_0 <- (min+max)/2 #mid point of bin
      idx <- which(actual==1) #TRUE values
      predicted_1 <- predicted[idx] #values predicted to be 1
      p_0 <- (sum(predicted_1)+m_0)/(length(predicted_1)+1) #intuitive prior for beta smoothing

      n <- length(actual) #how many elements in the bin
      n1 <- sum(actual) #how many actual y==1 in the bin
      n0 <- n-n1 #how many actual y==0 in the bin
      P <- (n1+p_0)/(n+1)

      if(n1>0){
        logLikelihood <- logLikelihood+n1*log(P)
      }
      if(n0>0){
        logLikelihood<-logLikelihood+n0*log(1-P)
      }

      histModel_all[[b]] <- list(min=min, max=max, n=n, n1=n1, n0=n0, P=P, midpoint=m_0)
    }

    else {
      Yhat <- predicted
      Y <- actual
      c <- floor(length(Y)/b) #number of elements per bin b to get equal frequency bins
      rest <- length(Y)-(c*b) #how many elements are not counted due to the floor() method
      i <- 1
      idx <- 1
      Tt <- list()
      idx2 <- 0

      while(i < b){ #i=Laufindex for bin number from 1 to last bin in the model
        #idx1 <- (i-1)*c+1 #lower idx of instance for bin i
        idx1 <- idx2+1

        if (i <= rest){ #rest elements are distributed evenly among the first
          idx2 <- idx1+c #upper idx of instance
        }
        else
          idx2 <- idx1+c-1

        j <- i+1
        while (j <= b){ #is the next bin j already the last bin b of the model?
          if (j < b){ #no!
            #Jidx2 <- j*c #lower idx of instance for bin j
            if (j <= rest){ #is bin j still supposed to contain one element more to avoid too full bin
              Jidx2 <- idx2+c #upper idx of instance
            }
            else{
              Jidx2 <- idx2+c-1
            }
            if (predicted[Jidx2]==predicted[idx1]){
              idx2 <- Jidx2
              j <- j+1
            }
            else
              break
          }
          else{
            Jidx2 <- N
            if (predicted[Jidx2]==predicted[idx1]){
              idx2 <- Jidx2
              j <- j+1
            }
            else
              break
          }
        }

        if (idx2<N){
          cutIdx$idx <- idx2
        }

        Tt[[idx]] <- list(Y=Y[idx1:idx2], PTR=predicted[idx1:idx2], Yhat=Yhat[idx1:idx2])
        idx <- idx+1
        i <- j
      }

      if (idx2<N){
        Tt[[idx]]<- list(Y=Y[(idx2+1):length(Y)], PTR=predicted[(idx2+1):length(Y)], Yhat=Yhat[(idx2+1):length(Y)])
      }

      b_0 <- b
      b <- length(Tt)
      histModel_all[[1]] <- list(min=0, max=(Tt[[1]]$Yhat[length(Tt[[1]]$Yhat)]+Tt[[2]]$Yhat[1])/2)
      cutPoints[1] <- histModel_all[[1]]$max #first cut point in binning model

      #intuitive prior for beta smooting
      m_0 <- (histModel_all[[1]]$min + histModel_all[[1]]$max)/2 #midpoint
      idx <- which(Tt[[1]]$Y==1) #Y==1 in bin 1
      PTR1 <- Tt[[1]]$PTR[idx] #which PTR have y==1 (in bin 1)
      p_0 <- (sum(PTR1)+m_0)/(length(PTR1)+1) #intuitive prior for beta smoothing

      #values for the first bin
      histModel_all[[1]]$n <- length(Tt[[1]]$Y) #how many values in first bin
      histModel_all[[1]]$n1 <- sum(Tt[[1]]$Y) #how many y==1 values in first bin
      histModel_all[[1]]$n0 <- histModel_all[[1]]$n-histModel_all[[1]]$n1 #how many y==0 values in first bin
      histModel_all[[1]]$P <- (histModel_all[[1]]$n1+p_0)/(histModel_all[[1]]$n+1)
      histModel_all[[1]]$P_observed <- (histModel_all[[1]]$n1)/(histModel_all[[1]]$n)
      histModel_all[[1]]$midpoint <- m_0

      if (histModel_all[[1]]$n1 > 0){
        logLikelihood <- logLikelihood + histModel_all[[1]]$n1*log(histModel_all[[1]]$P)
      }
      if (histModel_all[[1]]$n0 > 0){
        logLikelihood <- logLikelihood + histModel_all[[1]]$n0*log(1-histModel_all[[1]]$P)
      }

      #for second till second to last bin
      for (i in 2:(b-1)){
        if((b-1)<2){
          break
        }
        else{
          histModel_all[[i]] <- list()
          histModel_all[[i]]$min <- (Tt[[i]]$Yhat[1]+Tt[[i-1]]$Yhat[length(Tt[[i-1]]$Yhat)])/2 #min value in bin i
          histModel_all[[i]]$max <- (Tt[[i]]$Yhat[length(Tt[[i]]$Yhat)]+Tt[[i+1]]$Yhat[1])/2 #max value in bin i
          cutPoints[i] <- histModel_all[[i]]$max

          #intuitive prior for beta distribution smoothing
          m_0 <- (histModel_all[[i]]$min+histModel_all[[i]]$max)/2 #midpoint
          idx <- which(Tt[[i]]$Y==1) #Y==1 in bin i
          PTR1 <- Tt[[i]]$PTR[idx] #which PTR have y==1 (in bin i)
          p_0 <- (sum(PTR1)+m_0)/(length(PTR1)+1) #intuitive prior for beta smoothing

          histModel_all[[i]]$n <- length(Tt[[i]]$Y) #how many elements in bin i
          histModel_all[[i]]$n1 <- sum(Tt[[i]]$Y) #how many y==1 values in bin i
          histModel_all[[i]]$n0 <- histModel_all[[i]]$n-histModel_all[[i]]$n1 #how many y==0 values in bin i
          histModel_all[[i]]$P <- (histModel_all[[i]]$n1+p_0)/(histModel_all[[i]]$n+1) #intuitive prior for beta smoothing
          histModel_all[[i]]$P_observed <- (histModel_all[[i]]$n1)/(histModel_all[[i]]$n)
          histModel_all[[i]]$midpoint <- m_0

          if (histModel_all[[i]]$n1 > 0){
            logLikelihood <- logLikelihood + histModel_all[[i]]$n1*log(histModel_all[[i]]$P)
          }
          if (histModel_all[[i]]$n0 > 0){
            logLikelihood <- logLikelihood + histModel_all[[i]]$n0*log(1-histModel_all[[i]]$P)
          }
        }
      }

      #for last bin b of histModel_all
      histModel_all[[b]] <- list()
      histModel_all[[b]]$min <- (Tt[[b]]$Yhat[1]+Tt[[b-1]]$Yhat[length(Tt[[b-1]]$Yhat)])/2
      histModel_all[[b]]$max <- 1

      m_0 <- (histModel_all[[b]]$min + histModel_all[[b]]$max)/2 #midpoint
      idx <- which(Tt[[b]]$Y==1)
      PTR1 <- Tt[[b]]$PTR[idx]
      p_0 <- (sum(PTR1)+m_0)/(length(PTR1)+1) #intuitive prior for beta smoothing

      histModel_all[[b]]$n <- length(Tt[[b]]$Y)
      histModel_all[[b]]$n1 <- sum(Tt[[b]]$Y)
      histModel_all[[b]]$n0 <- histModel_all[[b]]$n - histModel_all[[b]]$n1
      histModel_all[[b]]$P <- (histModel_all[[b]]$n1+p_0)/(histModel_all[[b]]$n+1)
      histModel_all[[b]]$P_observed <- (histModel_all[[b]]$n1)/(histModel_all[[b]]$n)
      histModel_all[[b]]$midpoint <- m_0

      if (histModel_all[[b]]$n1 > 0){
        logLikelihood <- logLikelihood + histModel_all[[b]]$n1*log(histModel_all[[b]]$P)
      }
      if (histModel_all[[b]]$n0 > 0){
        logLikelihood <- logLikelihood + histModel_all[[b]]$n0*log(1-histModel_all[[b]]$P)
      }
    }
    return(list(histModel_all=histModel_all, cutIdx=cutIdx, cutPoints=cutPoints, logLL=logLikelihood))
  }

  elbow <- function(scores, alpha){

    #Assume R is the sorted Bayesian scores of histogram models
    #in a decreasing order. We fix a small number a > 0 (a = 0.001 in our experiments)
    #and pick the first ka associated binning models as the refined set of models,
    #where ka is a defined index in the sorted sequence where and o2=sigma2 is the empirical variance of the Bayesian scores.

    b <- length(scores)
    sigma2 <- (sqrt(mean(scores ^ 2) - mean(scores)^2))^2 #sd of sample mean is computed (denominator=N not N-1) and then ^2
    k <- 1 #laufindex zum scores durchsuchen
    idxs <- order(scores, decreasing=TRUE)
    R <- scores[idxs] #highest SV = rank 1, highest SV = lowest score

    while (R[k]==R[k+1]){ #scores unterscheiden sich nicht
      k <- k+1
    }
    while (k < b && ((R[k]-R[k+1]))/sigma2 > alpha){ #Differenz zwischen SV[n] und SV[n+1] ist hoch genug um in refined set aufgenommen zu werden
      k <- k+1
    }
    #for the first k elements of the sorted SV set(lowest score first) is the alpha high enough
    #those k elements in the refined SV set
    if (k > 1){
      res <- idxs[1:k-1]
    }
    else #k==1, include only highest Bayesion Scores in Averaging procedure
      res <- idxs[1]

    return(res)
  }

  processModel_local <- function(inModel, idxs){
    outModel <- list()
    for (i in 1:length(idxs)){
      outModel[[i]] <- inModel[[idxs[i]]]
    }
    outModel[[1]]$minScoreIdx <- 1 #best model is [[1]]
    outModel[[1]]$SV <- inModel[[1]]$SV[idxs]

    return(outModel=outModel)
  }

  #default options from paper are set as fixed values
  all <- data.frame(cbind(actual, predicted))

  N_0 <- 2 #default
  alpha <- 0.001 #default
  runSort <- 1 #default

   if (runSort==1){
    x <- order(predicted)
    predicted <- predicted[x]
    actual <- actual[x]
   }

  N <- length(predicted)
  lnfact <- initlnfact_local(N+1) #output: array with length+2 elements
  maxbinno <- min(ceiling(N/5), ceiling(10*N^(1/3))) #max. number of bins I can have in the model,  max=5
  minbinno <- max(1,floor(N^(1/3)/10)) #have at least 1 bin in binning model, min. number of bins, min=1
  MNM <- maxbinno-minbinno+1 #maximum number of possible binning models

  model <- list()
  model[[1]] <- list()
  model[[1]]$scoringFunc <- "BDeu2"
  opt1 <- list(PTR=predicted, lnfact=lnfact, N_0=N_0)

  for (b in 1:MNM){ #a binning model for each possible #bin b is created and evaulated using its BDeu2 score
    output_hist_calibration <- hist_calibration_freq_local(predicted, actual, b+minbinno-1)
    funcOpt <- buildFuncOpt_local(opt1, output_hist_calibration$histModel_all, output_hist_calibration$cutIdx,
                                  output_hist_calibration$cutPoints, output_hist_calibration$logLL)
    score <- get_BDeu_Score2_local(funcOpt) #should use the respective get_BDeu_Score(or BDeu2 (default)) method
    model[[b]] <- list(binNo=output_hist_calibration$histModel, cutIdx=output_hist_calibration$cutIdx,
                  cutPoints=output_hist_calibration$cutPoints, score=score, logLL=output_hist_calibration$logLL)
  }
  score <- c()
  logLL <- c()

  #Zusammenhang zwischen Score und LogLL..
  for (i in 1:MNM){
    score[i]<-model[[i]]$score
    logLL[i]<-model[[i]]$logLL
  }

  #which binning model hast the best BDeu2 score?
  maxScore <- -Inf
  maxScoreIdx <- 0
  minScore <- Inf
  minScoreIdx <- 0
  SV <- rep(0,MNM) #SV vector contains all scores for all evaluated models
  for (b in 1:MNM){
    SV[b] <- model[[b]]$score
    if(model[[b]]$score > maxScore){
      maxScoreIdx <- b
      maxScore <- model[[b]]$score
    }
    if (model[[b]]$score < minScore){
      minScoreIdx <- b
      minScore <- model[[b]]$score
    }
  }


  #SV becomes 1 for min(SV), SV becomes smallest for largest score, model with the logLL closest to 0
  SV <- exp((min(SV)-SV)/2)#SV=whole set of BDeu2 scores for each possible MNM
  model[[1]]$maxScoreIdx <- maxScoreIdx #first binning model (b=1) stores min and max ScoreIdxs
  model[[1]]$minScoreIdx <- minScoreIdx
  model[[1]]$SV <- SV

    #select only a number of models for averaging over the models
  idxs <- elbow(SV, alpha = alpha) #include the indexed SV scores in the refined/pruned model
  model2 <- processModel_local(model, idxs = idxs) #refined model

  p_observed <- c()
  p_calculated <- c()
  midpoint <- c()
  n <- c()
  n_1 <- c()

  for (i in 1:length(model2[[1]]$binNo)){
    p_observed[i]<- model2[[1]]$binNo[[i]]$P_observed
    p_calculated[i] <- model2[[1]]$binNo[[i]]$P #smoothed prevalence value
    midpoint[i]<- model2[[1]]$binNo[[i]]$midpoint
    n[i] <- model2[[1]]$binNo[[i]]$n
    n_1[i] <-model2[[1]]$binNo[[i]]$n1
  }
  bin_no <- seq(1,length(midpoint))

  #significance testing
  p_values_binom <- unlist(apply(cbind(n_1, n),1,binom_for_histogram)) #pvalues for single bins, binom.test
  binning_scheme <- data.frame(cbind(bin_no,midpoint,cases=n_1,all=n, prob_case=p_calculated,p_value=p_values_binom))

  for (i in 1:nrow(binning_scheme)){
    if(is.nan(binning_scheme[i,6])){
      binning_scheme[i,7] <- "no value"
    }

    else if(binning_scheme[i,6]<0.001){
      binning_scheme[i,7] <- "***"
    }

    else if(binning_scheme[i,6]<0.01){
      binning_scheme[i,7] <- "**"
    }

    else if(binning_scheme[i,6]<0.05){
      binning_scheme[i,7] <- "*"
    }
    else
      binning_scheme[i,7] <- "ns"
  }
  colnames(binning_scheme)[7] <- c("significance")

  #function has to return min/max for scaling
  min <- min(predicted)
  max <- max(predicted)

  #quality markers calibration model
  calibration_points <- binning_scheme$prob_case
  calibration_points_sign <- binning_scheme$p_value <0.05
  calibration_points_number <- length((binning_scheme$prob_case))
  calibration_points_number_sign <- length((subset(binning_scheme$prob_case, binning_scheme$p_value<0.05)))
  calibration_range <- range(binning_scheme$prob_case)
  if(sum(calibration_points_sign) != 0){
    calibration_range_sign <- range(subset(binning_scheme$prob_case, binning_scheme$p_value<0.05))
  }
  else{
    calibration_range_sign <- 0
  }


  return(bbq=list(type="BBQ", model=model, prunedmodel=model2,
                  binnning_scheme=binning_scheme, min=min, max=max,
                  calibration_points=list(calibration_points=calibration_points,calibration_points_sign=calibration_points_sign),
                  calibration_range=list(calibration_range=calibration_range, calibration_range_sign=calibration_range_sign),
                  calibration_points_number=list(calibration_points_number=calibration_points_number, calibration_points_number_sign=calibration_points_number_sign)))
}

