#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(
    navbarPage("DBSconvertR App - by Oliver Scherf-Clavel (c) 2022 - JMU Wuerzburg", id="mainpage",theme = shinytheme("united"),

               tabPanel("Import data",
                        sidebarLayout (
                          sidebarPanel(
                            fileInput("loadPT", "Load data", multiple = FALSE,
                                      accept = c(".xlsx", ".xls"),
                                      width = NULL, buttonLabel = "Browse...",
                                      placeholder = "No file selected"),
                            selectInput("conc_unit_blood", "Blood concentration unit", selected = 1, choices = c("ng/mL", "mg/L", "mmol/L", "nmol/L", "µg/L")),
                            selectInput("conc_unit_plasma", "Plasma concentration unit", selected=1, choices = c("ng/mL", "mg/L", "mmol/L", "nmol/L", "µg/L")),
                            downloadButton("download_dataset", "Download dataset")
                            ),
                            mainPanel(
                              DT::dataTableOutput("raw_data_table")
                            )
                          )

                        ),

               tabPanel("Explore data",
                            sidebarLayout(
                                sidebarPanel(
                                  selectInput("method",
                                              "Method:",
                                              selected=1,
                                              choices = c("Method 1", "Method 2", "Method 3", "Method 4", "Method 5")
                                              ),
                                  conditionalPanel(condition="input.method=='Method 1'",
                                                   sliderInput("fu_slider",
                                                               "Fraction unbound in plasma(Fu)",
                                                               min = 0,
                                                               max = 1,
                                                               value = 0.5),
                                                   numericInput("rho_slider",
                                                               "Affinity to blood cells (Rho)",
                                                               min = 0,
                                                               max = 10,
                                                               value=0.5,
                                                               step = 0.1)
                                  ),
                                  conditionalPanel(condition="input.method=='Method 2'",
                                                   numericInput("K_bp",
                                                                "Blood cells/Plasma partition coefficient",
                                                                value = 1,
                                                                min=0,
                                                                step = .1)
                                  ),
                                  conditionalPanel(condition="input.method=='Method 2'|input.method=='Method 1'",
                                                   checkboxInput("use_mean_hct",
                                                                 "Use mean Hct",
                                                                 value=FALSE
                                                   )
                                  ),
                                  sliderInput("limits_bland_altman",
                                              "Acceptance limits for Bland Altman Analysis",
                                              min = -100, max=100,
                                              value = c(-20, 20)),
                                  actionButton("submit", label = "Analyze Data", icon = icon("chart-bar"), width = NULL)


                                ),

                                # Show a plot of the generated distribution
                                mainPanel(

                                  use_busy_spinner(spin = "fading-circle"),

                                  tabsetPanel(
                                    tabPanel("Observations",
                                             plotOutput("observedPlot")
                                    ),
                                    tabPanel("Bland Altman",
                                             plotOutput("blandAltmanPlot")
                                    ),
                                    tabPanel("Passing Bablok Regression",
                                             plotOutput("pbPlot")
                                    ),
                                    tabPanel("Deming Regression",
                                             plotOutput("demPlot")
                                    ),
                                  )



                                )
                            )
                   ),
               tabPanel("Create Report",
                        sidebarLayout (
                          sidebarPanel(
                            downloadButton("download_report", "Download Report")
                          ),
                          mainPanel(use_busy_spinner(spin = "fading-circle")

                          )
                        )
               )
          )
    )

