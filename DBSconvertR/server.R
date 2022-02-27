#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # reactive Data currently used
    app_data <- reactiveValues(
        current_dataset = NULL,
        current_bland_altman_plot = NULL,
        current_obs_plot = NULL,
        current_passing_bablok_plot = NULL,
        current_deming_plot = NULL,
        PB.reg = NULL,
        dem.reg = NULL,
        current_passing_bablok_plot_2 = NULL,
        current_deming_plot_2 = NULL,
        PB.reg_2 = NULL,
        dem.reg_2 = NULL
    )

    output$raw_data_table <- DT::renderDataTable({

        return(app_data$current_dataset)


    })

    output$observedPlot <- renderPlot({

        app_data$current_obs_plot

    })

    output$blandAltmanPlot <- renderPlot({

        app_data$current_bland_altman_plot

    })

    output$pbPlot <- renderPlot({

        gridExtra::grid.arrange(app_data$current_passing_bablok_plot,
                                app_data$current_passing_bablok_plot_2, ncol=2)
    })

    output$demPlot <- renderPlot({

        gridExtra::grid.arrange(app_data$current_deming_plot,
                                app_data$current_deming_plot_2, ncol=2)
    })

    ## Load an Excel file with data
    observeEvent(input$loadPT,{
        inFile <- input$loadPT

        if (is.null(inFile))
            return(NULL)

        tryCatch({

            openxlsx::read.xlsx(inFile$datapath) -> app_data$current_dataset
        },
        error = function(e){
            showModal(modalDialog(
                title = "ERROR",
                HTML(paste("File not recognized!<br>Details:<br><br>",e)),
                easyClose = TRUE,
                footer = NULL
            ))
        })

    })

    observeEvent(input$submit, {

        if(is.null(app_data$current_dataset)){
            showModal(modalDialog(
                title = "ERROR",
                HTML("No data entry present!"),
                easyClose = TRUE,
                footer = NULL
            ))
            return()
        }

        app_data$current_dataset %>%
            ggplot(aes(x=dbs, y=plasma)) +
            theme_bw() +
            plot_theme +
            labs(x=paste0("Plasma concentration [",input$conc_unit_plasma,"]"),
                 y=paste0("DBS concentration [",input$conc_unit_blood,"]")) +
            geom_point(size=5,shape=1) -> app_data$current_obs_plot

        app_data$dem.reg <- mcreg(app_data$current_dataset$plasma,
                                  app_data$current_dataset$dbs,
                                  method.reg = "Deming")

        app_data$PB.reg <- mcreg(app_data$current_dataset$plasma,
                                 app_data$current_dataset$dbs,
                                 method.reg = "PaBa")



        app_data$current_dataset %>% mutate(pred_plasma=convertBloodToPlasma(conc_blood= app_data$current_dataset$dbs,
                                                                             conc_plasma= app_data$current_dataset$plasma,
                                                                             hct=app_data$current_dataset$hct,
                                                                             rho=input$rho_slider,
                                                                             fu=input$fu_slider,
                                                                             pb = app_data$PB.reg,
                                                                             dem = app_data$dem.reg,
                                                                             K_bp = input$K_bp,
                                                                             method=input$method)) -> app_data$current_dataset

        app_data$current_dataset %>%
            createBlandAltmanPlot("pred_plasma", "plasma",
                                  lowerLimit = input$limits_bland_altman[1],
                                  upperLimit = input$limits_bland_altman[2]) +
            plot_theme -> app_data$current_bland_altman_plot



        app_data$dem.reg_2 <- mcreg(app_data$current_dataset$plasma,
                                    app_data$current_dataset$pred_plasma,
                                    method.reg = "Deming")

        app_data$PB.reg_2 <- mcreg(app_data$current_dataset$plasma,
                                   app_data$current_dataset$pred_plasma,
                                   method.reg = "PaBa")

        createRegPlot(x=app_data$PB.reg, "plasma", "dbs", x_lab = paste0("Plasma concentration [",input$conc_unit_plasma,"]"), y_lab=paste0("DBS concentration [",input$conc_unit_blood,"]")) + plot_theme -> app_data$current_passing_bablok_plot

        createRegPlot(x=app_data$dem.reg, "plasma", "dbs", x_lab = paste0("Plasma concentration [",input$conc_unit_plasma,"]"), y_lab=paste0("DBS concentration [",input$conc_unit_blood,"]")) + plot_theme-> app_data$current_deming_plot

        createRegPlot(x=app_data$PB.reg_2, "plasma", "pred_plasma", x_lab = paste0("Plasma concentration [",input$conc_unit_plasma,"]"), y_lab=paste0("Predicted plasma concentration [",input$conc_unit_plasma,"]")) + plot_theme-> app_data$current_passing_bablok_plot_2

        createRegPlot(x=app_data$dem.reg_2, "plasma", "pred_plasma", x_lab = paste0("Plasma concentration [",input$conc_unit_plasma,"]"), y_lab=paste0("Predicted plasma concentration [",input$conc_unit_plasma,"]")) + plot_theme-> app_data$current_deming_plot_2
    })
})


