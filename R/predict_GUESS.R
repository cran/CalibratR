#' @title predict_GUESS
#' @description returns calibrated predictions for the instances \code{new} using the trained GUESS calibration model \code{build_guess_object}.
#' Two different evaluation methods are available.
#' Method 1: returns the p-value for the score \code{new} under the distribution that is handed over in the \code{build_guess_object}
#' Method 2: returns the probability density value for the score \code{new} under the distribution that is handed over in the \code{build_guess_object}
#' @param build_guess_object output from the \code{\link{build_GUESS}} method
#' @param new vector of uncalibrated probabilities
#' @param density_evaluation which density evaluation method should be used to infer calculate probabilities, Default: 2
#' @param return_class_density if set to TRUE, class densities p(x|class) are returned, Default: FALSE
#' @return a list object containing the following components:
#' \item{predictions}{contains a vector of calibrated predictions}
#' \item{pred_idx}{which density evaluation method was used}
#' \item{significance_test_set}{the percentage of \code{new} instances that was evaluated using significant prediction estimates}
#' \item{dens_case}{a vector containing the p(x|case) values}
#' \item{dens_control}{a vector containing the p(x|control) values}
#' @details \code{dens_case} and \code{dens_control} are only returned when \code{return_class_density} is set to TRUE
#' @rdname predict_GUESS


predict_GUESS <- function(build_guess_object, new, density_evaluation=2, return_class_density=FALSE){
  ###local function####
  evaluate_density_1 <- function(distr, new){

    pdistname <- distr$pdistname #use pdistname

    estimate <- distr$estimate
    list_estimate <- list()

    for(i in 1:length(estimate)){
      list_estimate[i] <- estimate[i]
    }
    p_case <- do.call(match.fun(pdistname), c(new,list_estimate))

    return(p_case)
  }

  evaluate_density_2 <- function(distr, new){

    ddistname <- distr$ddistname

    estimate <- distr$estimate
    list_estimate <- list()

    for(i in 1:length(estimate)){
      list_estimate[i] <- estimate[i]
    }
    density <- do.call(match.fun(ddistname), c(new,list_estimate))

    return(density)
  }
  new_1 <- new
  new_2 <- new
  out <- rep(0, length(new))
  dens_cases <- rep(0, length(new))
  dens_controls  <- rep(0, length(new))
  best_fit_cases <- build_guess_object$best_fit_cases
  best_fit_controls <- build_guess_object$best_fit_controls
  class_probs <- build_guess_object$class_probs

  #scale input first if data was z-scaled for t distribution
  if(best_fit_cases$distname=="t"){
    new_1 <- scale(new, center = build_guess_object$mean_case, scale=F)

  }
  if(best_fit_controls$distname=="t"){
    new_2 <- scale(new, center = build_guess_object$mean_control, scale=F)

  }

  #calculate P(C)
    class_prob_case <- class_probs$cases
    class_prob_control <- class_probs$controls

  #for rebalancing set both priors to 0.5
    #class_prob_case <- 0.5
    #class_prob_control <- 0.5

  #which evaluation method should be used to determine p-value
    method <- switch(density_evaluation,
           "1"= evaluate_density_1,
           "2"= evaluate_density_2)

    for (i in 1:length(new)){

        #for evaluation of P(x|Case)
        dens_case <- method(best_fit_cases, new_1[i])

        if(density_evaluation==1){
          #for evaluation of P(x|Control) for GUESS1: 1-pnorm = probability, for x to be a control.
          dens_control <- (1-method(best_fit_controls, new_2[i]))
        }
        else{
          dens_control <- method(best_fit_controls, new_2[i])
        }

        #P(x|C)*P(C)
        path_prob_case <- dens_case*class_prob_case
        path_prob_control <- dens_control*class_prob_control

        #p(x)
        evidence <- path_prob_case+path_prob_control

        if(evidence==0){ #to avoid dividing by 0
          prob_case <- path_prob_case #if evidence is 0: both path_probs are set to 0 by default
          prob_control <- path_prob_control
        }
        else{
        #path_prob/p(x)
          prob_case <- path_prob_case/evidence
          prob_control <- path_prob_control/evidence
        }

        class_guess <- max(prob_case, prob_control)
        class <- switch(which.max(c(prob_case, prob_control)), "1"=1, "2"=0)

        out[i] <- prob_case
        dens_cases[i] <- dens_case
        dens_controls[i] <- dens_control
      }

    #significance
    estimate_case <- best_fit_cases$estimate
    list_estimate_case <- list()

    for(i in 1:length(estimate_case)){
      list_estimate_case[i] <- estimate_case[i]
    }

    estimate_control <- best_fit_controls$estimate
    list_estimate_control <- list()

    for(i in 1:length(estimate_control)){
      list_estimate_control[i] <- estimate_control[i]
    }

    #significant results in test set
    sign_test_set <- sum(new>build_guess_object$t_crit[[1]]& new<build_guess_object$t_crit[[2]])/length(new)



    if(return_class_density==TRUE){
      return(list(predictions=out, dens_case=dens_cases, dens_controls=dens_controls,
                  pred_idx=density_evaluation, significance_test_set=sign_test_set))
    }

    else
      return(list(predictions=out, pred_idx=density_evaluation, significance_test_set=sign_test_set))

}
