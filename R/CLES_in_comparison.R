
#' @title get_CLE_comparison
#' @description visualises how class 1 and class 0 classification error (CLE) differs in each trained calibration model.
#' Comparing class-specific CLE helps to choose a calibration model for applications were classification error is cost-sensitive for one class.
#' See \code{\link{get_CLE_class}} for details on the implementation.
#' @param list_models list object that contains all error values for all trained calibration models. For the specific format, see the calling function \code{\link{visualize_calibratR}}.
#' @return ggplot2
#' @rdname get_CLE_comparison

get_CLE_comparison <- function(list_models){

  list_models$original <- NULL
  list_errors_0 <- list()
  list_errors_1 <- list()
  idx <- 1


  for (j in list_models){
    list_errors_1[[names(list_models)[[idx]]]] <- j$CLE_class_1
    list_errors_0[[names(list_models)[[idx]]]] <- j$CLE_class_0
    idx <- idx+1
  }
  df_cle_0 <- cbind(reshape2::melt(list_errors_0), Class="CLE class 0")
  df_cle_1 <- cbind(reshape2::melt(list_errors_1), Class="CLE class 1")
  df <- rbind(df_cle_0, df_cle_1)
  Class <- NULL
  value <- NULL
  L1 <- NULL
  ggplot2::ggplot(df, ggplot2::aes(x=L1, y=value, colour=Class)) +
    ggplot2::ggtitle("Class-specific CLE") +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::geom_boxplot() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))
}
