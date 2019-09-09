#' Plot a heartrate profile
#'
#' \code{plot_ecg} plots a heat rate profile.
#'
#' @param data A data.frame with columns timestamp (POSIXct) and ecg (numeric)
#' @param max_points Maximum number of points to decimate \code{data} (10,000
#' by default)
#' @param interactive Should the plot be interactive? FALSE by default.
#' Requires package "plotly".
#' @return A gpplot object if not interactive, a plotly object otherwise.
#' @export
plot_ecg <- function(data, max_points = 1e4, interactive = FALSE) {
  # Check inputs
  assertthat::assert_that(is.data.frame(data))
  assertable::assert_colnames(data, c("timestamp", "ecg"), only_colnames = FALSE)
  assertthat::assert_that(is.numeric(max_points))
  assertthat::assert_that(length(max_points) == 1)
  assertthat::assert_that(max_points > 0)
  max_points <- as.integer(max_points)

  if (interactive && !requireNamespace("plotly", quietly = TRUE)) {
    stop("Package \"plotly\" must be installed for interactive plots")
  }

  # Plot heart rate profile
  p <- data %>%
    dplyr::slice(seq(1, nrow(data), length.out = max_points)) %>%
    ggplot2::ggplot(ggplot2::aes(timestamp, ecg)) +
    ggplot2::geom_line(size = 0.5) +
    ggplot2::scale_x_datetime(date_labels = "%H:%M") +
    ggplot2::theme_classic()

  if (interactive) {
    plotly::ggplotly(p)
  } else {
    p
  }
}
