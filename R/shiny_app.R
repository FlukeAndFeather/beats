# Also see shiny_helpers.R

# UI
hr_ui <- shiny::fillPage(
  shiny::tags$script('
    $(document).on("keypress", function (e) {
              Shiny.onInputChange("keypress", e.which);
              });
              '),
  shiny::sidebarLayout(
    shiny::sidebarPanel(shiny::radioButtons("mode",
                                            "Interaction mode",
                                            list(`Add heart beat` = 1,
                                                 `Clear heart beat` = 2,
                                                 `Add gap` = 3,
                                                 `Clear gap` = 4,
                                                 `Set beat threshold` = 5)),
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

    # When brushing, remove downstream brushes and data
    shiny::observeEvent(input$data_brush, {
      session$resetBrush("deploy_brush")
      session$resetBrush("detail_brush")
    })
    shiny::observeEvent(input$deploy_brush, {
      session$resetBrush("detail_brush")
    })

    # Finish and return beats
    shiny::observeEvent(input$done, {
      shiny::stopApp(returnValue = prepare_beats(values$heart_beats, values$ecg_gaps))
    })

    output$data_plot <- shiny::renderPlot({
      c(deploy, p) %<-% plot_profile(this_data = values$ecg_data,
                                     box = input$data_brush)
      values$ecg_deploy <- deploy
      p
    })
    output$deploy_plot <- shiny::renderPlot({
      c(detail, p) %<-% plot_profile(this_data = values$ecg_deploy,
                                     box = input$deploy_brush,
                                     beats = values$heart_beats)
      values$ecg_detail <- detail
      p
    })
    output$detail_plot <- shiny::renderPlot({
      c(p, beats, gaps) %<-% plot_detail(values$ecg_detail,
                                         shiny::isolate(values$heart_beats),
                                         shiny::isolate(values$ecg_gaps),
                                         input$detail_click,
                                         input$detail_brush,
                                         shiny::isolate(input$mode))
      values$heart_beats <- beats
      values$ecg_gaps <- gaps
      p
    })
    output$output_csv <- shiny::downloadHandler(
      filename = function() {
        format(lubridate::now(), "ecg_%y%m%d%H%M%S.csv")
      },
      content = function(file) {
        readr::write_csv(prepare_csv(values$heart_beats,
                                     values$ecg_gaps),
                         file)
      })
  })
}
