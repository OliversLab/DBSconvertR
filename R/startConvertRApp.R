#' Starts the shiny App to analyze data
#' @returns nothing
#' @examples
#' startConvertRApp()
#' @import shiny
#' @import shinybusy
#' @import openxlsx
#' @import ggplot2
#' @import tidyverse
#' @import mcr
#' @import shinythemes
#' @import DT
#' @import gridExtra
#' @import shinybusy
#' @import rmarkdown
#' @import kableExtra
#' @export

startConvertRApp <- function() {
  shiny::runApp('DBSconvertR')
}
