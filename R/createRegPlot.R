
#' Create a plot with confidence intervals from mcr::MCResultResampling
#' @param x data.frame with plasma concentration and estimated plasma concentration
#' @param conc_ref column name of the measured plasma concentration
#' @param conc_test comlumn name of the estimated plasma concentration
#' @returns The result is a ggplot object
#' @examples
#' createRegPlot()
#' @import ggplot2
#' @import mcr
#' @import tidyverse
#' @export

createRegPlot <- function(x = NULL, x_name=NULL, y_name=NULL, n_x_sim = 100, alpha=0.05, x_lab, y_lab) {

  #PB.reg@bootcimeth
  reg_method <- paste0("Regression: ", x@regmeth)

  # --- create confidence interval ---

    # get the lower and upper limit of the x values in x
    range_x <- range(x@data[, "x"], na.rm = TRUE)
    # create points to simulate
    x_sim <- seq(range_x[1], range_x[2], length.out = n_x_sim)
    # spacing between two simulated points
    delta <- abs(range_x[1] - range_x[2])/n_x_sim
    # add 10 points at the upper end and at the lower end of the regression line
    x_sim_extended <- c(x_sim[1] - delta * 1:10, x_sim, x_sim[length(x_sim)] + delta * 1:10)
    # calculate the confidence interval for the selected points
    ci_limits <- calcResponse(x, alpha = alpha, x.levels = x_sim_extended) %>% data.frame()

  # --- end create confidence interval ---


  mc_res_data <- x

  intercept <- paste0(round(mc_res_data@para[1,1],1), " (CI: ",round(mc_res_data@para[1,3],1), " to ", round(mc_res_data@para[1,4],1), ")")
  slope <- paste0(round(mc_res_data@para[2,1],3), " (CI: ",round(mc_res_data@para[2,3],3), " to ", round(mc_res_data@para[2,4],3), ")")

  cor_text_1 <- paste0("Pearson's r = ", round(cor(mc_res_data@data$x, mc_res_data@data$y, method="pearson"),4))
  cor_text_2 <- paste0("Kendall's ", "tau", " = ", (round(cor(mc_res_data@data$x, mc_res_data@data$y, method="kendall"),4)), sep = "")
  cor_text_3 <- paste0("Spearman's ", "rho"," = ", (round(cor(mc_res_data@data$x, mc_res_data@data$y, method="spearman"),4)), sep = "")

  ci_lab <- paste0( round((1-alpha)*100,0), "% CI" )

  ggplot(mc_res_data@data, aes(x=x,y=y)) +
    ggtitle(paste("Intercept:",intercept, "\nSlope:", slope),
            paste(cor_text_1, cor_text_2, cor_text_3)) +
    theme_bw() +
    geom_point(size=5, shape=1) +
    geom_abline(mapping = aes(intercept=0, slope=1, colour="identity"), linetype=2) +
    geom_abline(mapping = aes(intercept=mc_res_data@glob.coef[1], slope=mc_res_data@glob.coef[2], colour=reg_method)) +
    theme(legend.position = "bottom") +
    geom_ribbon(data=ci_limits, aes(x=X, ymin=Y.LCI, ymax=Y.UCI, y=NULL, fill=ci_lab), alpha=.2) +
    scale_colour_manual(values = c("black", "red"),
                        guide = guide_legend(override.aes = list(
                          linetype =  c(2,1),
                          shape = c(NA, NA),
                          fill = c("white", "white")
                        ))) +
    scale_fill_manual(values = c("red")) +
    coord_cartesian(expand = FALSE) +
    labs(x=x_lab, y=y_lab, colour="", fill="")

}
