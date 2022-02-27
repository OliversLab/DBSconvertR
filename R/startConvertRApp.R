#' Starts the shiny App to analyze data
#' @returns nothing
#' @examples
#' startConvertRApp()
#' @import shiny
#' @import mcr
#' @import shinythemes
#' @import DT
#' @export

startConvertRApp <- function() {
  shiny::runApp('DBSconvertR')
}
