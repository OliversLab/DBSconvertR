library(openxlsx)
library(ggplot2)
library(tidyverse)
library(DBSconvertR)
library(mcr)
library(shinythemes)
library(DT)
library(gridExtra)

# ---- color scheme and plot theme ---

main_plot_col <- "#E95420"

limit_plot_col <-"#990000"
text_plot_col <- "grey20"
cont_plot_col <- "lightgrey"
backg_plot_col <-"white"
line_plot_col <- "gray"
plot_grid_col <- "gray"

plot_theme <- theme(axis.text.x = element_text(colour=text_plot_col,size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
                    axis.text.y = element_text(colour=text_plot_col,size=12,angle=0,hjust=1,vjust=0,face="plain"),
                    axis.title.x = element_text(colour=text_plot_col,size=14,angle=0,hjust=.5,vjust=0,face="bold"),
                    axis.title.y = element_text(colour=text_plot_col,size=14,angle=90,hjust=0.5,vjust=2,face="bold"),
                    legend.position = "bottom", legend.justification = c(0,1), legend.text=element_text(size=13), legend.title = element_text(size=13),
                    panel.background = element_rect(fill = backg_plot_col, colour = cont_plot_col), panel.border = element_blank(), panel.grid.major = element_line(colour = plot_grid_col, linetype = 2),
                    panel.grid.minor = element_line(colour = plot_grid_col, linetype = 2), axis.line = element_line(colour = line_plot_col))
