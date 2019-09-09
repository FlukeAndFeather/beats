#' Find heart rate from ECG data
#'
#' \code{find_hr} is an interactive tool for processing ECG data to get heart
#' rate profiles. Use this tool to identify individual heart beats and gaps in
#' the ECG data.
#'
#' @param data A data.frame with columns timestamp (POSIXct) and ecg (numeric)
#' @return A data.frame with columns
#' * timestamp (POSIXct)
#' * ecg (numeric)
#' * period_s (numeric)
#' * freq_hz (numeric)
#' * freq_bpm (numeric)
#' @md
#' @export
find_hr <- function(data) {
  options(shiny.maxRequestSize = 100*1024^2)
  shiny::runApp(list(ui = hr_ui, server = hr_server(data)))
}
