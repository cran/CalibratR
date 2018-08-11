#' @title visualize_error_boxplot
#' @description compares error values among different calibration models. A boxplots is created from the n error values that were obtained during the n-times repeated Cross-Validation procedure.
#' Different error values are implemented and can be compared:
#' \cr discrimination error = sensitivity, specificity, accuracy, AUC (when \code{discrimination}=TRUE)
#' \cr calibration error = ece, mce, rmse, class 0 cle, class 1 cle (when \code{discrimination}=FALSE)
#' For the calculation of the errors, see the respective methods listed in the "see also" section
#' @param list_models list object that contains all error values for all trained calibration models. For the specific format, see the calling function \code{\link{visualize_calibratR}}.
#' @param discrimination boolean (TRUE or FALSE). If TRUE, discrimination errors are compared between models; if FALSE calibration error is compared, Default: TRUE
#' @return An object of class list, with the following components:
#' \cr if \code{discrimination}=TRUE
#' \item{sens}{ggplot2 boxplot that compares all evaluated calibration models with regard to sensitivity.}
#' \item{spec}{ggplot2 boxplot that compares all evaluated calibration models with regard to specificity}
#' \item{acc}{ggplot2 boxplot that compares all evaluated calibration models with regard to accuracy}
#' \item{auc}{ggplot2 boxplot that compares all evaluated calibration models with regard to AUC}
#' \item{list_errors}{list object that contains all discrimination error values that were used to construct the boxplots}
#' \cr if \code{discrimination}=FALSE
#' \item{ece}{ggplot2 boxplot that compares all evaluated calibration models with regard to expected calibration error}
#' \item{mce}{ggplot2 boxplot that compares all evaluated calibration models with regard to maximum expected calibration error (MCE)}
#' \item{rmse}{ggplot2 boxplot that compares all evaluated calibration models with regard to root mean square error (RMSE)}
#' \item{cle_0}{ggplot2 boxplot that compares all evaluated calibration models with regard to class 0 classification error (CLE)}
#' \item{cle_1}{ggplot2 boxplot that compares all evaluated calibration models with regard to class 1 classification error (CLE)}
#' \item{list_errors}{list object that contains all calibration error values that were used to construct the boxplots}
#' @seealso
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{ggtitle}},\code{\link[ggplot2]{scale_x_discrete}},\code{\link[ggplot2]{geom_boxplot}},\code{\link[ggplot2]{theme}},\code{\link[ggplot2]{element_text}}
#'  \code{\link[reshape2]{melt}},\code{\link{get_CLE_class}},\code{\link{getECE}},\code{\link{getMCE}},\code{\link{getRMSE}}, \code{\link{evaluate_discrimination}}
#' @rdname visualize_error_boxplot
#' @importFrom ggplot2 ggplot aes ggtitle scale_x_discrete geom_boxplot theme element_text
#' @importFrom reshape2 melt

visualize_error_boxplot <- function(list_models, discrimination=TRUE){

  idx <- 1
  list_errors <- list()

  if(discrimination){
    list_errors[["sensitivity"]] <- list()
    list_errors[["specificity"]] <- list()
    list_errors[["accuracy"]] <- list()
    list_errors[["auc"]] <- list()


    for (j in list_models){
      list_errors[["sensitivity"]][[names(list_models)[[idx]]]] <- j$sens
      list_errors[["specificity"]][[names(list_models)[[idx]]]] <- j$spec
      list_errors[["accuracy"]][[names(list_models)[[idx]]]] <- j$acc
      list_errors[["auc"]][[names(list_models)[[idx]]]] <- j$auc
      idx <- idx+1
    }

    df_sens <- data.frame(list_errors$sensitivity)
    variable <- NULL
    value <- NULL

    p1 <- ggplot2::ggplot(reshape2::melt(df_sens, id.vars=NULL), ggplot2::aes(x=variable, y=value)) +
      ggplot2::ggtitle("Sensitivity") +
      ggplot2::scale_x_discrete(name = NULL) +
      ggplot2::geom_boxplot(fill="#4271AE", alpha=0.7) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))

    df_spec <- data.frame(list_errors$specificity)

    p2 <- ggplot2::ggplot(reshape2::melt(df_spec, id.vars=NULL), ggplot2::aes(x=variable, y=value)) +
      ggplot2::ggtitle("Specificity") +
      ggplot2::scale_x_discrete(name = NULL) +
      ggplot2::geom_boxplot(fill="#4271AE", alpha=0.7) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))

    df_acc <- data.frame(list_errors$accuracy)

    p3 <- ggplot2::ggplot(reshape2::melt(df_acc, id.vars=NULL), ggplot2::aes(x=variable, y=value)) +
      ggplot2::ggtitle("Accuracy") +
      ggplot2::scale_x_discrete(name = NULL) +
      ggplot2::geom_boxplot(fill="#4271AE", alpha=0.7) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))

    df_auc <- data.frame(list_errors$auc)

    p4 <- ggplot2::ggplot(reshape2::melt(df_auc, id.vars=NULL), ggplot2::aes(x=variable, y=value)) +
      ggplot2::ggtitle("AUC") +
      ggplot2::scale_x_discrete(name = NULL) +
      ggplot2::geom_boxplot(fill="#4271AE", alpha=0.7) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))

    return(list(sens=p1, spec=p2, acc=p3, auc=p4, list_errors=list_errors))
  }


  else{

    list_models$original <- NULL
    list_errors[["ece"]] <- list()
    list_errors[["mce"]] <- list()
    list_errors[["rmse"]] <- list()
    list_errors[["cle_class1"]] <- list()
    list_errors[["cle_class0"]] <- list()



    for (j in list_models){
    list_errors[["ece"]][[names(list_models)[[idx]]]] <- j$ECE_equal_width
    list_errors[["mce"]][[names(list_models)[[idx]]]] <- j$MCE_equal_width
    list_errors[["rmse"]][[names(list_models)[[idx]]]] <- j$RMSE
    list_errors[["cle_class1"]][[names(list_models)[[idx]]]] <- j$CLE_class_1
    list_errors[["cle_class0"]][[names(list_models)[[idx]]]] <- j$CLE_class_0
    idx <- idx+1
    }

    df_ece <- data.frame(list_errors$ece)
    variable <- NULL
    value <- NULL

      p1 <- ggplot2::ggplot(reshape2::melt(df_ece, id.vars=NULL), ggplot2::aes(x=variable, y=value)) +
        ggplot2::ggtitle("ECE") +
        ggplot2::scale_x_discrete(name = NULL) +
        ggplot2::geom_boxplot(fill="#4271AE", alpha=0.7) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))

      df_mce <- data.frame(list_errors$mce)

      p2 <- ggplot2::ggplot(reshape2::melt(df_mce, id.vars=NULL), ggplot2::aes(x=variable, y=value)) +
        ggplot2::ggtitle("MCE") +
        ggplot2::scale_x_discrete(name = NULL) +
        ggplot2::geom_boxplot(fill="#4271AE", alpha=0.7) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))

      df_rmse <- data.frame(list_errors$rmse)

      p3 <- ggplot2::ggplot(reshape2::melt(df_rmse, id.vars=NULL), ggplot2::aes(x=variable, y=value)) +
        ggplot2::ggtitle("RMSE") +
        ggplot2::scale_x_discrete(name = NULL) +
        ggplot2::geom_boxplot(fill="#4271AE", alpha=0.7) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))

      df_cle_0 <- data.frame(list_errors$cle_class0)

      p4 <- ggplot2::ggplot(reshape2::melt(df_cle_0, id.vars=NULL), ggplot2::aes(x=variable, y=value)) +
        ggplot2::ggtitle("CLE class 0") +
        ggplot2::scale_x_discrete(name = NULL) +
        ggplot2::geom_boxplot(fill="#4271AE", alpha=0.7) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))

      df_cle_1 <- data.frame(list_errors$cle_class1)

      p5 <- ggplot2::ggplot(reshape2::melt(df_cle_1, id.vars=NULL), ggplot2::aes(x=variable, y=value)) +
        ggplot2::ggtitle("CLE class 1") +
        ggplot2::scale_x_discrete(name = NULL) +
        ggplot2::geom_boxplot(fill="#4271AE", alpha=0.7) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))

      return(list(ece=p1, mce=p2, rmse=p3, cle_0=p4, cle_1=p5, list_errors=list_errors))
    }
  }
