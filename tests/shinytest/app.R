shiny::shinyApp(ui = beats:::hr_ui,
                server = beats:::hr_server(beats::ecg_bw190918_62R),
                options = list(shiny.testmode = TRUE))
