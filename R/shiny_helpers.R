# plot_profile
# Input: plot data and brush box
# Output: brushed data and plot
# c(deploy, plot) %<-% plot_profile(values$ecg_data, input$data_brush)
plot_profile <- function(this_data, box, beats = NULL) {
  sub_data <- NULL
  # If no data yet, return a blank plot
  if (is.null(this_data))
    return(list(sub_data, ggplot2::ggplot(NULL)))

  # Maximum 10,000 points
  data_sparse <- this_data %>%
    dplyr::slice(seq(1, nrow(this_data), length.out = 1e4))

  # Date breaks
  date_rng <- max(data_sparse$timestamp) - min(data_sparse$timestamp)
  offset <- 0.05
  date_breaks <- seq(min(data_sparse$timestamp) + offset * date_rng,
                     max(data_sparse$timestamp) - offset * date_rng,
                     length.out = 5)

  # Plot
  p <- ggplot2::ggplot(data_sparse, ggplot2::aes(timestamp, ecg)) +
    ggplot2::geom_line(size = 0.1) +
    ggplot2::scale_x_datetime(breaks = date_breaks) +
    ggplot2::ylim(0, 4095) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  # Brushed data
  if (!is.null(box)) {
    sub_data <- dplyr::filter(this_data,
                              dplyr::between(timestamp, box$xmin, box$xmax))
  }

  # Add points for heart beats
  if (!is.null(beats)) {
    beats_visible <- dplyr::filter(beats,
                                   dplyr::between(timestamp,
                                                  dplyr::first(this_data$timestamp),
                                                  dplyr::last(this_data$timestamp)))
    p <- p + ggplot2::geom_point(data = beats_visible, shape = 3, color = "red")
  }

  list(sub_data, p)
}

# plot_detail
# Input: plot data
# Output: plot
#output$detail_plot <- renderPlot({ plot_detail(values$ecg_detail) })
plot_detail <- function(data, beats, gaps, click, brush, mode) {
  tryCatch({
    # If no data yet, return a blank plot
    if (is.null(data))
      return(list(NULL, beats, gaps))

    # Maximum 10,000 points
    data_sparse <- data %>%
      dplyr::slice(seq(1, nrow(data), length.out = 1e4))

    # Which heart beats and ECG gaps are visible?
    if (is.null(beats)) {
      beats <- dplyr::filter(data, FALSE)
    }
    if (is.null(gaps)) {
      # Create a dummy table. No rows, time zone set to UTC.
      gaps <- tibble::tibble(timestamp_begin = lubridate::now("UTC"),
                             timestamp_end = lubridate::now("UTC")) %>%
        dplyr::filter(FALSE)
    }
    beats_visible <- dplyr::filter(beats,
                                   dplyr::between(timestamp,
                                                  min(data$timestamp),
                                                  max(data$timestamp)))
    gaps_visible <- dplyr::filter(gaps,
                                  timestamp_end > min(data$timestamp),
                                  timestamp_begin < max(data$timestamp))
    if (nrow(gaps_visible) > 0) {
      gaps_visible <- gaps_visible %>%
        dplyr::mutate(gap_xmin = pmax(min(data$timestamp), timestamp_begin),
                      gap_xmax = pmin(max(data$timestamp), timestamp_end),
                      gap_ymin = min(data$ecg),
                      gap_ymax = max(data$ecg))
    } else {
      gaps_visible <- gaps_visible %>%
        dplyr::mutate(gap_xmin = NA,
                      gap_xmax = NA,
                      gap_ymin = NA,
                      gap_ymax = NA)
    }

    # Add/remove beats/gaps
    if (!is.null(click)) {
      nearest_ecg <- which.min(abs(click$x - as.numeric(data$timestamp)))
      if (mode == 1) { # Add beat
        search_range <- dplyr::filter(data,
                                      timestamp > data$timestamp[nearest_ecg] - 0.5,
                                      timestamp < data$timestamp[nearest_ecg] + 0.5)
        nearest_peak <- dplyr::filter(search_range, ecg == max(ecg)) %>% dplyr::slice(1)
        if (!nearest_peak$timestamp %in% beats$timestamp)
          beats <- rbind(beats, nearest_peak)
      } else if (mode == 2) { # Clear beat
        if (nrow(beats_visible) > 0) {
          nearest_beat <- which.min(abs(click$x - as.numeric(beats_visible$timestamp)))
          drop_timestamp <- beats_visible$timestamp[nearest_beat]
          if (abs(click$x - as.numeric(drop_timestamp)) < 0.5) {
            beats <- dplyr::filter(beats, timestamp != drop_timestamp)
          }
        }
      } else if (mode == 4) { # clear gap
        gap <- which(click$x > gaps$timestamp_begin & click$x < gaps$timestamp_end)
        if (length(gap) == 1)
          gaps <- dplyr::slice(gaps, -gap)
      } else if (mode == 5) { # set beat threshold
        # Look for peaks in 1s windows
        window_size <- 100
        local_peaks <- zoo::rollapply(data$ecg,
                                      window_size,
                                      function(x) which.max(x) == window_size / 2,
                                      partial = TRUE)
        local_peaks[data$ecg < click$y] <- FALSE
        beats <- rbind(beats, data[local_peaks,])
      }
    } else if (!is.null(brush)) {
      if (mode == 3) { # add gap
        nearest_ecg <- integer(2)
        nearest_ecg[1] <- which.min(abs(brush$xmin - as.numeric(data$timestamp)))
        nearest_ecg[2] <- which.min(abs(brush$xmax - as.numeric(data$timestamp)))
        gaps <- rbind(gaps,
                      tibble::tibble(timestamp_begin = data$timestamp[nearest_ecg[1]],
                                     timestamp_end = data$timestamp[nearest_ecg[2]]))
      }
    }

    # Collapse overlapping gaps
    merge_gaps <- function(.gaps, i) {
      # Merge a gap with the one after it
      .gaps$timestamp_end[i] <- max(.gaps$timestamp_end[c(i, i + 1)])
      dplyr::slice(.gaps, -(i + 1))
    }
    gaps <-dplyr:: arrange(gaps, timestamp_begin)
    overlaps <- gaps$timestamp_end >= dplyr::lead(gaps$timestamp_begin,
                                                  default = dplyr::last(data$timestamp + 1))
    while (any(overlaps)) {
      overlap <- dplyr::first(which(overlaps))
      gaps <- merge_gaps(gaps, overlap)
      overlaps <- gaps$timestamp_end >= dplyr::lead(gaps$timestamp_begin,
                                                    default = dplyr::last(data$timestamp + 1))
    }

    # Create plot
    # Align time zone with profile plots
    tz <- lubridate::tz(data_sparse$timestamp)
    if (tz == "")
      tz <- Sys.timezone()
    p <- ggplot2::ggplot(data_sparse, ggplot2::aes(timestamp, ecg)) +
      ggplot2::geom_line(size = 0.1) +
      ggplot2::geom_point(data = beats_visible,
                          shape = 3,
                          color = "red") +
      ggplot2::geom_rect(ggplot2::aes(xmin = gap_xmin,
                                      xmax = gap_xmax,
                                      ymin = gap_ymin,
                                      ymax = gap_ymax),
                         gaps_visible,
                         inherit.aes = FALSE,
                         fill = "blue",
                         alpha = 0.1) +
      ggplot2::scale_x_datetime(date_labels = "%H:%M:%S", timezone = tz) +
      ggplot2::ylim(0, 4095) +
      ggplot2::labs(title = sprintf("beats:%d;gaps:%d", nrow(beats), nrow(gaps))) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title = ggplot2::element_blank())
    list(p, beats, gaps)
  }, warning = function(e) browser())
}

# prepare_beats
# Input: beats, gaps
# Output: data frame
prepare_beats <- function(beats, gaps) {
  result <- beats %>%
    dplyr::mutate(period_s = as.numeric(dplyr::lead(timestamp) - timestamp,
                                        unit = "secs"),
                  freq_hz = 1 / period_s,
                  freq_bpm = freq_hz * 60)
  unk_beats <- purrr::map_int(gaps$timestamp_begin,
                              ~ which.max(beats$timestamp[beats$timestamp < .x]))
  result[unk_beats, -1] <- NA
  result
}
# prepare_rdata
# Input: ecg_deploy, beats, gaps
# Output:
prepare_rds <- function(data, deploy, detail, beats, gaps) {
  list(data = data,
       deploy = deploy,
       detail = detail,
       beats = beats,
       gaps = gaps,
       output = prepare_beats(beats, gaps))
}
prepare_csv <- function(beats, gaps) {
  prepare_beats(beats, gaps)
}
