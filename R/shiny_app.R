# Also see shiny_helpers.R

# UI
hr_ui <- shiny::fillPage(
  shiny::sidebarLayout(
    shiny::sidebarPanel(shiny::radioButtons("mode",
                                            "Interaction mode",
                                            list(`Add heart beat` = "add_beat",
                                                 `Clear heart beat` = "clear_beat",
                                                 `Add gap` = "add_gap",
                                                 `Clear gap` = "clear_gap",
                                                 `Set beat threshold` = "beat_thr")),
                        shiny::downloadButton("output_csv", "Download heart beats (.csv)"),
                        shiny::actionButton("done", "Finish and return data.frame")),
    shiny::mainPanel(
      shiny::plotOutput("data_plot",
                        brush = "data_brush",
                        height = "100px"),
      shiny::plotOutput("deploy_plot",
                        brush = "deploy_brush",
                        height = "100px"),
      shiny::plotOutput("detail_plot",
                        click = "detail_click",
                        brush = "detail_brush",
                        height = "400px"))
  )
)


# SERVER
hr_server <- function(data) {
  shiny::shinyServer(function(input, output, session) {
    values <- shiny::reactiveValues(ecg_data = data)

    # When brushing profiles, remove downstream brushes and update data
    shiny::observeEvent(input$data_brush, {
      session$resetBrush("deploy_brush")
      session$resetBrush("detail_brush")
      values$ecg_deploy <- subdata(shiny::isolate(values$ecg_data),
                                   input$data_brush)
    })
    shiny::observeEvent(input$deploy_brush, {
      session$resetBrush("detail_brush")
      values$ecg_detail <- subdata(shiny::isolate(values$ecg_deploy),
                                    input$deploy_brush)
    })

    # Handle clicking and brushing on detail plot
    shiny::observeEvent(input$detail_click, {
      result <- handle_detail_click(shiny::isolate(values$ecg_detail),
                                    input$detail_click,
                                    shiny::isolate(input$mode),
                                    shiny::isolate(values$heart_beats),
                                    shiny::isolate(values$ecg_gaps))
      # Update heart_beats and ecg_gaps
      values$heart_beats <- result$heart_beats
      values$ecg_gaps <- result$ecg_gaps
    })
    shiny::observeEvent(input$detail_brush, {
      result <- handle_detail_brush(shiny::isolate(values$ecg_detail),
                                    input$detail_brush,
                                    shiny::isolate(input$mode),
                                    shiny::isolate(values$heart_beats),
                                    shiny::isolate(values$ecg_gaps))
      # Update heart_beats and ecg_gaps
      values$heart_beats <- result$heart_beats
      values$ecg_gaps <- result$ecg_gaps
    })

    # Data plot (profile of all data)
    output$data_plot <- shiny::renderPlot(plot_profile(values$ecg_data))
    # Deploy plot (profile of deployment data)
    output$deploy_plot <- shiny::renderPlot(plot_profile(values$ecg_deploy,
                                                         values$heart_beats,
                                                         values$ecg_gaps))
    # Detail plot (zoomed in data)
    output$detail_plot <- shiny::renderPlot(plot_profile(values$ecg_detail,
                                                         values$heart_beats,
                                                         values$ecg_gaps,
                                                         detail = TRUE))

    # Download CSV
    output$output_csv <- shiny::downloadHandler(
      filename = function() {
        format(lubridate::now(), "ecg_%y%m%d%H%M%S.csv")
      },
      content = function(file) {
        readr::write_csv(prepare_csv(values$heart_beats,
                                     values$ecg_gaps),
                         file)
      })

    # Finish and return beats
    shiny::observeEvent(input$done, {
      shiny::stopApp(returnValue = prepare_beats(values$heart_beats,
                                                 values$ecg_gaps))
    })
  })
}
