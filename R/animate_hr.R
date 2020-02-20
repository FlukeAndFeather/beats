#' Animate ECG data and heart beats
#'
#' \code{animate_hr} produces a GIF of heart beats in (close to) real time.
#'
#' @param ecg A data.frame with columns timestamp (POSIXct) and ecg (numeric)
#' (see \code{\link{read_ube}})
#' @param beats A data.frame with columns timestamp (POSIXct), ecg, period_s,
#' freq_hz, and freq_bpm (see \code{\link{find_hr}})
#' @param which_beats Indices of the heart beats (in param \code{beats}) to be
#' animated. If NULL (default), all beats will be animated.
#' @param big If FALSE (default) then throw an error if the animation will last
#' more than 20s. This prevents R from crashing if it tries to animate too
#' much.
#' @return A \code{\link[gganimate]{gif_image}} object
#'
#' @examples
#' \donttest{
#'   animate_hr(ecg_bw190918_62R, beats_bw190918_62R, which_beats = 44:50, big = TRUE)
#' }
#'
#' @md
#' @export
animate_hr <- function(ecg, beats, which_beats = NULL, big = FALSE) {
  # Check if required packages are installed
  required_packages <- c("gganimate", "gifski", "png")
  if (any(!required_packages %in% utils::installed.packages())) {
    stop("To use animate_hr install gganimate, gifski, and png")
  }

  # Check input data
  if (!all(c("timestamp", "ecg") %in% colnames(ecg))) {
    stop("Columns of table ecg must include 'timestamp' and 'ecg'.")
  }
  if (!all(c("timestamp", "ecg", "freq_bpm") %in% colnames(beats))) {
    stop("Columns of table beats must include 'timestamp', 'ecg', and 'freq_bpm'.")
  }
  if (!is.null(which_beats)) {
    if (!is.numeric(which_beats))
      stop("which_beats must be numeric")
    if (any(!which_beats %in% seq_along(beats$timestamp)))
      stop("which_beats must be valid indices of beats")
  }
  if (!is.logical(big) || length(big) != 1)
    stop("big must be a logical of length 1")

  # If which_beats is NULL, animate all heart beats
  if (is.null(which_beats)) {
    which_beats = seq_along(beats$timestamp)
  }

  # gganimate requires data.frames, not tibbles
  beats2 <- dplyr::slice(beats, which_beats) %>%
    dplyr::mutate(bpm_lbl = sprintf("%0.2f", freq_bpm)) %>%
    as.data.frame()

  # Add a 0.5 buffer on either side of the heart beats for ecg data, expanded
  # to the nearest second. gganimate requires whole seconds.
  buffer <- .5 # seconds
  ecg_start <- lubridate::floor_date(beats2$timestamp[1] - buffer,
                                     unit = "second")
  ecg_end <- lubridate::ceiling_date(beats2$timestamp[nrow(beats2)] + buffer,
                                     unit = "second")
  ecg2 <- dplyr::filter(ecg,
                        dplyr::between(timestamp, ecg_start, ecg_end)) %>%
    as.data.frame()

  # Calculate duration of animation, fps, and nframes
  dur_s <- as.numeric(max(ecg2$timestamp) - min(ecg2$timestamp), unit = "secs")
  if (dur_s %% 1 != 0)
    stop("Something went wrong, dur_s should be a whole number")
  if (!big && dur_s > 20) {
    stop("For large animations (>20s) big must be TRUE")
  }
  fps <- 10
  nframes <- dur_s * fps

  # Animate heart rate
  hr_anim <- ggplot2::ggplot(ecg2, ggplot2::aes(timestamp, ecg)) +
    ggplot2::geom_line(size = 0.1) +
    ggplot2::geom_point(ggplot2::aes(group = seq_along(timestamp)),
                        data = beats2,
                        shape = 3,
                        color = "red") +
    ggplot2::geom_text(ggplot2::aes(group = seq_along(timestamp),
                                    label = bpm_lbl),
                       data = beats2,
                       nudge_y = 100) +
    ggplot2::scale_x_datetime("", date_labels = "%H:%M:%S", timezone = Sys.timezone()) +
    ggplot2::ylim(0, 4095) +
    ggplot2::theme_minimal() +
    gganimate::transition_reveal(timestamp)
  gganimate::animate(hr_anim, nframes, fps, dur_s)
}
