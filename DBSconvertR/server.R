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
        dem.reg_2 = NULL,
        current_settings = NULL
    )

    reset_app <- function() {

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
        app_data$current_bland_altman_plot = NULL
        app_data$current_obs_plot = NULL
        app_data$current_passing_bablok_plot = NULL
        app_data$current_deming_plot = NULL
        app_data$PB.reg = NULL
        app_data$dem.reg = NULL
        app_data$current_passing_bablok_plot_2 = NULL
        app_data$current_deming_plot_2 = NULL
        app_data$PB.reg_2 = NULL
        app_data$dem.reg_2 = NULL
        app_data$current_settings = NULL
    }

    output$raw_data_table <- DT::renderDataTable({

        return(app_data$current_dataset)


    })

    output$observedPlot <- renderPlot({

        if(is.null(app_data$current_obs_plot))
            return()

        app_data$current_obs_plot

    })

    output$blandAltmanPlot <- renderPlot({

        if(is.null(app_data$current_bland_altman_plot))
            return()

        app_data$current_bland_altman_plot

    })

    output$pbPlot <- renderPlot({

        if(is.null(app_data$current_passing_bablok_plot))
            return()

        gridExtra::grid.arrange(app_data$current_passing_bablok_plot,
                                app_data$current_passing_bablok_plot_2, ncol=2)
    })

    output$demPlot <- renderPlot({

        if(is.null(app_data$current_deming_plot))
            return()

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

        show_spinner() # show the spinner

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


        if(input$use_mean_hct) {
            temp_hct <- mean(app_data$current_dataset$hct)

        } else {
            temp_hct <- app_data$current_dataset$hct
        }

        if(input$method=="Method 1") {
            app_data$current_settings <- paste0("Fraction unbound in plasma (fu): ", input$fu_slider,
                                                "  \n", "Blood cell affinity (rho): ", input$rho_slider,
                                                "  \n", "Use mean Hct: ", ifelse(input$use_mean_hct,"yes", "no"),
                                                "  \n", "Mean Hct: ", round(mean(app_data$current_dataset$hct),2)
            )
        } else if (input$method=="Method 2") {
            app_data$current_settings <- paste0("Blood cell/Plasma partition coefficient:", input$K_bp,
                                                "  \n", "Use mean Hct: ", ifelse(input$use_mean_hct,"yes", "no"),
                                                "  \n", "Mean Hct:", round(mean(app_data$current_dataset$hct),2)
            )

        } else if (input$method=="Method 3") {
            app_data$current_settings <- paste0("Used the mean DBS/Plasma ratio")

        } else if (input$method=="Method 4") {
            app_data$current_settings <- paste0("Used the coefficients of Passing Bablok regression",
                                                "  \n", "Slope: ", round(app_data$PB.reg@para[2,1],3), " (CI: ",round(app_data$PB.reg@para[2,3],3), " to ", round(app_data$PB.reg@para[2,4],3), ")",
                                                "  \n", "Intercept: ", round(app_data$PB.reg@para[1,1],1), " (CI: ",round(app_data$PB.reg@para[1,3],1), " to ", round(app_data$PB.reg@para[1,4],1), ")")


        } else if (input$method=="Method 5") {
            app_data$current_settings <- paste0("Used the coefficients of Deming regression",
                                                "  \n", "Slope: ", round(app_data$dem.reg@para[2,1],3), " (CI: ",round(app_data$dem.reg@para[2,3],3), " to ", round(app_data$dem.reg@para[2,4],3), ")",
                                                "  \n", "Intercept: ", round(app_data$dem.reg@para[1,1],1), " (CI: ",round(app_data$dem.reg@para[1,3],1), " to ", round(app_data$dem.reg@para[1,4],1), ")")


        }



        app_data$current_dataset %>% mutate(pred_plasma=convertBloodToPlasma(conc_blood= app_data$current_dataset$dbs,
                                                                             conc_plasma= app_data$current_dataset$plasma,
                                                                             hct=temp_hct,
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

        hide_spinner() # remove spinner
    })

    # reset app if something changes
    observeEvent(input$limits_bland_altman, {
        reset_app()
    })
    observeEvent(input$conc_unit_blood, {
        reset_app()
    })
    observeEvent(input$conc_unit_plasma, {
        reset_app()
    })
    observeEvent(input$method, {
        reset_app()
    })
    observeEvent(input$fu_slider, {
        reset_app()
    })
    observeEvent(input$rho_slider, {
        reset_app()
    })
    observeEvent(input$K_bp, {
        reset_app()
    })
    observeEvent(input$use_mean_hct, {
        reset_app()
    })

    # download the data.set
    output$download_dataset <- downloadHandler(

        filename = function() {

            if (is.null(input$loadPT))
                return(NULL)

            paste0(Sys.Date(),"_",input$loadPT, sep = "")
        },
        content = function(file) {


            tryCatch({

                openxlsx::write.xlsx(app_data$current_dataset, file)


            },
            error = function(e){
                showModal(modalDialog(
                    title = "ERROR",
                    HTML(paste("Error while saving Patient!<br>Details:<br><br>",e)),
                    easyClose = TRUE,
                    footer = NULL
                ))
                stop(safeError(e))
            })
        }
    )

    # create and download report

    output$download_report = downloadHandler(

        filename = paste("report_", Sys.Date(), ".pdf", sep=""),
        content = function(file) {

            if(is.null(app_data$current_obs_plot))
                return()

            show_spinner() # show the spinner

            out <- render('report.Rmd', output_format=pdf_document(latex_engine = "xelatex"))
            file.rename(out, file)

            hide_spinner() # remove the spinner

        }
    )
})


