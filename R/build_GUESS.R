#' @title build_GUESS
#' @description This method builds a GUESS calibration model using the trainings set provided.
#' @param actual vector of observed class labels (0/1)
#' @param predicted vector of uncalibrated predictions
#' @return returns the trained GUESS model that can be used to calibrate a test set using the \code{\link{predict_GUESS}} method
#' @seealso
#'  \code{\link[fitdistrplus]{denscomp}}
#' @rdname build_GUESS
#' @export
#' @importFrom fitdistrplus denscomp
#' @importFrom stats median



build_GUESS <- function(actual, predicted){
  ###local functions###
  get_class_prob <- function(cases, controls){
    #calculates proportion of cases and controls in data set to determine P(C)
    n_cases <- length(cases)
    n_controls <- length(controls)
    all <- n_cases+n_controls
    prob_cases <- n_cases/all
    prob_controls <- n_controls/all

    return(list(cases=prob_cases, controls=prob_controls))
  }
  get_LL <- function(distributions){
    #ranks tested distributions by their logLL
    #returns ranked list of distributions, and distribution with maximal LL
    ll <- c()
    log_list <- c()

    extract_ll <- function (distribution){
      ll <- distribution$loglik
    }

    plot_ll <- function (distribution){
      fitdistrplus::llplot(distribution)
    }

    log_list <- lapply(distributions, extract_ll)
    max <- which.max(unlist(log_list)) #find max logLL

    return(list(distribution=log_list, best_fit=distributions[max][[1]]))
  }

  distribution_test <- function(data){

    bool=0

    z_scaled_data <- scale(data, center=TRUE, scale=F)[,1]
    data_fit_norm <- fitdistrplus::fitdist(data, "norm") #normal distribution
    data_fit_log <- fitdistrplus::fitdist(data, "logis") #logistic distribution
    data_fit_t_z_trans <-  fitdistrplus::fitdist(z_scaled_data, "t", start=list(df=10)) #t distribution

    #test for distributions that only take input >=0
    if(sum(data<0) == 0){
      data <- data[!data==0] #fitting works better if data does not include 0 value (?)
      data_fit_exp <- fitdistrplus::fitdist(data, "exp") #exponential distribution
      data_fit_w  <-  fitdistrplus::fitdist(data, "weibull", start=list(shape=5, scale=0.5)) #weibull distribution
      data_fit_g  <-  fitdistrplus::fitdist(data, "gamma") #gamma distribution
      data_fit_ln <-  fitdistrplus::fitdist(data, "lnorm") #lognormal distribution

      names <- c("exponential", "Weibull","gamma", "lognormal")
      bool=1
    }

    else{
      names <- c()
      distributions <- c()
    }

    if(bool==1){
      summary <- list(summary(data_fit_norm),summary(data_fit_log), summary(data_fit_exp), summary(data_fit_w), summary(data_fit_g), summary(data_fit_ln), summary(data_fit_t_z_trans))
      best_fit <- get_LL(summary)
      return(list(distributions=summary, best_fit=best_fit$best_fit))
    }

    else{
      summary <- list(summary(data_fit_norm),summary(data_fit_log), summary(data_fit_t_z_trans))
      best_fit <- get_LL(summary)
      return(list(distributions=summary, best_fit=best_fit$best_fit))
    }
  }
  all <- data.frame(cbind(actual, predicted))
  controls <- subset(all[,2],all[,1]==0)
  cases <- subset(all[,2],all[,1]==1)

  #function has to return min/max for scaling and mean and sd for Z-transformation for t-distribution fitting
  min <- min(predicted)
  max <- max(predicted)
  mean_cases <- mean(cases)
  sd_cases <- sd(cases)
  mean_controls <- mean(controls)
  sd_controls <- sd(controls)

  #evaluate possible distributions for the two classes
  distr_cases <- distribution_test(cases)
  distr_controls <- distribution_test(controls)

  #save distribution with max logLL in best_fit_Xx
  best_fit_cases <- distr_cases$best_fit
  best_fit_controls <- distr_controls$best_fit
  dist_name_cases <- best_fit_cases$qdistname
  dist_name_controls <- best_fit_controls$qdistname

  #plot best distributions for cases and controls
  fit_cases <- fitdistrplus::denscomp(best_fit_cases, main="Best fit: Cases",
                         legendtext=best_fit_cases$distname, demp=TRUE, datacol="firebrick3", plotstyle="ggplot")
  fit_controls <- fitdistrplus::denscomp(best_fit_controls, main="Best fit: Controls",
                         legendtext=best_fit_controls$distname, demp=TRUE, datacol="darkolivegreen4", plotstyle="ggplot")

  #get class probabiliy P(C)
  class_probs <- get_class_prob(cases, controls)

  #significance testing
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

  #define critical values (5%, 95%) for both distributions
  t_crit_cases_l <- do.call(match.fun(dist_name_cases), c(0.05,list_estimate_case))
  t_crit_cases_u <-  do.call(match.fun(dist_name_cases), c(0.95,list_estimate_case))
  t_crit_controls_l <- do.call(match.fun(dist_name_controls), c(0.05,list_estimate_control))
  t_crit_controls_u <- do.call(match.fun(dist_name_controls), c(0.95,list_estimate_control))

  #define critical values where both distributions are in their 5% most extreme values
  t_crit <- c(crit_case_l=t_crit_cases_l,
              crit_control_l=t_crit_controls_l,
              crit_case_u=t_crit_cases_u,
              crit_control_u=t_crit_controls_u)
  #significant results in test set (lower than lower bound of controls and higher than upper bound of cases)
  sign_train_set <- sum(predicted>t_crit[[2]]& predicted<t_crit[[3]])/length(predicted)




  return(list(type="GUESS", best_fit_cases=best_fit_cases, best_fit_controls=best_fit_controls, class_probs=class_probs, min=min, max=max,
              mean_cases=mean_cases, sd_cases=sd_cases, mean_controls=mean_controls, sd_controls=sd_controls, t_crit=t_crit, sign_train_set=sign_train_set,
              plot=list(cases=fit_cases,controls=fit_controls)))
}
