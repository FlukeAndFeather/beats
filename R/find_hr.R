#' Find heart rate from ECG data
#'
#' \code{find_hr} is an interactive tool for processing ECG data to get heart
#' rate profiles. Use it to identify individual heart beats and gaps in ECG
#' the ECG data.
#'
#' @param data A data.frame with columns timestamp (POSIXct) and ecg (numeric)
#' @return A data.frame with columns
#'   * timestamp (time of heart beat, POSIXct)
#'   * ecg (raw value of ECG peak)
#'   * period_s (period in seconds to next heart beat)
#'   * freq_hz (heart rate in Hz)
#'   * freq_bpm (heart rate in beats per minute)
#' @details The GUI is divided into a side panel with controls and a main panel
#'   with plots. The side panel has controls for choosing the interaction mode,
#'   exporting results, and exiting. The main panel shows up to three plots for
#'   viewing and interacting with the data. As you interact with the plots,
#'   more detailed views appear.
#'
#'   1. The top plot shows the entire ECG profile, including data before and
#'     after deployment. Brush (click and drag) the part of the profile with
#'     the deployment itself.
#'   2. The middle plot is a profile of the deployment. Brush it to zoom in to
#'     shorter periods so you can see individual heart beats.
#'   3. The bottom plot shows data in the most detailed view. Clicking and
#'     brushing the data will add/clear heart beats and gaps.
#'
#'   There are five interaction modes. In "Add heart beat" mode, clicking will
#'   add a heart beat on the nearest peak (should be the QRS complex). "Clear
#'   heart beat" will remove the nearest beat. Use "Add gap" to brush areas
#'   with unusable data, to avoid artificially low heart rates. Overlapping gaps
#'   are merged automatically. In "Clear gap" mode, click on gaps to clear
#'   them. "Set beat threshold" adds heart beats in bulk. Clicking will add a
#'   heart beat to every peak higher than the click point. This will only add
#'   beats to the part of the data visible in the detail plot.
#'
#'   There are two buttons at the bottom of the side panel for when you're done
#'   using the tool. "Download heart beats" exports the heart rate data to CSV
#'   and "Finish and return data.frame" quits the tool and returns the result.
#'
#' @examples
#' if (interactive()) {
#'   # File path to sample data
#'   fp <- system.file("extdata", "max_ecg_190826b.ube", package = "beats")
#'   # Read ube file
#'   ecg_data <- read_ube(fp)
#'   # Launch GUI
#'   heartbeats <- find_hr(ecg_data)
#'   # In this heart rate profile, zoom into 12:53:20 - 12:54:25 to see actual beats
#' }
#' @md
#' @export
find_hr <- function(data) {
  options(shiny.maxRequestSize = 100*1024^2)
  # hr_ui and hr_server are in shiny_app.R
  shiny::runApp(list(ui = hr_ui, server = hr_server(data)))
}
