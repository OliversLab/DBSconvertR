
#' Create a plot with prediction intervals from mrgsim result and optional observed data
#' @param x data.frame with plasma concentration and estimated plasma concentration
#' @param conc_ref column name of the measured plasma concentration
#' @param conc_test comlumn name of the estimated plasma concentration
#' @returns The result is a ggplot object
#' @examples
#' createBlandAltmanPlot()
#' @import ggplot2
#' @import tidyverse
#' @export

createBlandAltmanPlot <- function(x = NULL,
                                  conc_ref = NULL,
                                  conc_test = NULL,
                                  upperLimit=20,
                                  lowerLimit=-20,
                                  x_axis_title ="Mean (Ref+Test)/2 [ng/mL]",
                                  y_axis_title ="Relative deviation (Ref-Test)/Mean [%]") {

  df_x <- data.frame(conc_ref = x[[conc_ref]], conc_test=x[[conc_test]]) %>%
    mutate(mean=(conc_ref+conc_test)/2,
           delta_abs = conc_ref-conc_test,
           deta_rel = 100*(conc_ref-conc_test)/mean,
           in_range = (deta_rel < upperLimit) & (deta_rel > lowerLimit))

  df_sd <- sd(df_x$deta_rel)
  df_mean <- mean(df_x$deta_rel)

  percent_in_range <- round(sum(df_x$in_range)/nrow(df_x),3)*100

  ggplot(df_x, aes(x=mean, y=deta_rel)) +
    theme_bw() +
    ggtitle(paste0(percent_in_range, " % of pairs within limits")) +
    geom_point(aes(fill=in_range), size=5, shape=21) +
    geom_hline(yintercept = 0, linetype=2, size=1)+
    geom_hline(mapping=aes(colour="mean", yintercept = df_mean), size=1)+
    geom_hline(mapping=aes(colour="Mean \u00B1 2 x SD", yintercept = df_mean+2*df_sd), size=1) +
    geom_hline(mapping=aes(colour="Mean \u00B1 2 x SD", yintercept = df_mean-2*df_sd), size=1) +
    geom_hline(mapping=aes(colour="limits", yintercept = upperLimit), size=1) +
    geom_hline(mapping=aes(colour="limits", yintercept = lowerLimit), size=1) +
    scale_fill_manual(values=c("FALSE" = "red","TRUE"= "green")) +
    scale_colour_manual(values = c("red", "black", "blue")) +
    labs(x=x_axis_title, y=y_axis_title, colour="", fill="Within limits")

}



