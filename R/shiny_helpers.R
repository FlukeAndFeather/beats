# Plot an ECG profile
plot_profile <- function(data, beats = NULL, gaps = NULL, detail = FALSE) {
  shiny::req(data)

  # Maximum 10,000 points
  data_sparse <- data %>%
    dplyr::slice(seq(1, nrow(data), length.out = 1e4))

  # Date breaks
  date_rng <- max(data_sparse$timestamp) - min(data_sparse$timestamp)
  offset <- 0.05
  date_breaks <- seq(min(data_sparse$timestamp) + offset * date_rng,
                     max(data_sparse$timestamp) - offset * date_rng,
                     length.out = 5)

  # Profile plots show dates in ymd hms, detail is just hms
  tz <- lubridate::tz(data_sparse$timestamp)
  if (tz == "")
    tz <- Sys.timezone()
  if (detail) {
    lbl <- "%H:%M:%S"
  } else {
    lbl <- "%y-%m-%d %H:%M"
  }
  x_scale <- ggplot2::scale_x_datetime(date_labels = lbl,
                                       timezone = tz,
                                       breaks = date_breaks)

  # Plot
  p <- ggplot2::ggplot(data_sparse, ggplot2::aes(timestamp, ecg)) +
    ggplot2::geom_line(size = 0.1) +
    x_scale +
    ggplot2::ylim(0, 4095) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  # Add points for heart beats
  if (!is.null(beats)) {
    beats_visible <- dplyr::filter(beats,
                                   dplyr::between(timestamp,
                                                  dplyr::first(data$timestamp),
                                                  dplyr::last(data$timestamp)))
    p <- p + ggplot2::geom_point(data = beats_visible,
                                 shape = 3,
                                 color = "red")
  }

  # Add rects for gaps
  if (!is.null(gaps)) {
    gaps_visible <- dplyr::filter(gaps,
                                  timestamp_end > min(data$timestamp),
                                  timestamp_begin < max(data$timestamp))
    if (nrow(gaps_visible) > 0) {
      gaps_visible <- gaps_visible %>%
        dplyr::mutate(gap_xmin = pmax(min(data$timestamp), timestamp_begin),
                      gap_xmax = pmin(max(data$timestamp), timestamp_end))
      p <- p + ggplot2::geom_rect(ggplot2::aes(xmin = gap_xmin,
                                               xmax = gap_xmax,
                                               ymin = 0,
                                               ymax = 4095),
                                  gaps_visible,
                                  inherit.aes = FALSE,
                                  fill = "blue",
                                  alpha = 0.1)
    }
  }

  p
}

# Handle clicking on detail plot
handle_detail_click <- function(data, click, mode, beats, gaps) {
  # Wait for data and a click
  shiny::req(data, click)
  switch (mode,
          add_beat = add_beat(data, click, beats, gaps),
          clear_beat = clear_beat(data, click, beats, gaps),
          clear_gap = clear_gap(data, click, beats, gaps),
          beat_thr = beat_thr(data, click, beats, gaps),
          list(heart_beats = beats,
               ecg_gaps = gaps)
  )
}

# Handle brushing the detail plot
handle_detail_brush <- function(data, brush, mode, beats, gaps) {
  # Wait for data and a brush
  shiny::req(data, brush)
  switch (mode,
          add_gap = add_gap(data, brush, beats, gaps),
          list(heart_beats = beats,
               ecg_gaps = gaps)
  )
}

# Add heart beat
add_beat <- function(data, click, beats, gaps) {
  nearest_ecg <- which.min(abs(click$x - as.numeric(data$timestamp)))
  # Look for a peak within 0.5s
  search_range <- dplyr::filter(data,
                                timestamp > data$timestamp[nearest_ecg] - 0.5,
                                timestamp < data$timestamp[nearest_ecg] + 0.5)
  nearest_peak <- dplyr::filter(search_range, ecg == max(ecg)) %>%
    dplyr::slice(1)
  # Add peak to beats
  new_beats <- rbind(nearest_peak, beats) %>%
    # Sort chronologically
    dplyr::arrange(timestamp) %>%
    # Remove duplicates
    dplyr::distinct()

  # Return beats and gaps
  list(heart_beats = new_beats,
       ecg_gaps = gaps)
}

# Clear heart beat
clear_beat <- function(data, click, beats, gaps) {
  new_beats <- beats

  # Filter beats to visible in details
  beats_visible <- dplyr::filter(beats,
                                 dplyr::between(timestamp,
                                                dplyr::first(data$timestamp),
                                                dplyr::last(data$timestamp)))

  # If any beats are visible...
  if (nrow(beats_visible) > 0) {
    nearest_beat <- which.min(abs(click$x - as.numeric(beats_visible$timestamp)))
    drop_timestamp <- beats_visible$timestamp[nearest_beat]
    # ... drop the closest one within 0.5s
    if (abs(click$x - as.numeric(drop_timestamp)) < 0.5) {
      new_beats <- dplyr::filter(beats, timestamp != drop_timestamp)
    }
  }

  # Return beats and gaps
  list(heart_beats = new_beats,
       ecg_gaps = gaps)
}

# Add a gap
add_gap <- function(data, brush, beats, gaps) {
  # Nearest points to brush borders define the new gap
  nearest_ecg <- integer(2)
  nearest_ecg[1] <- which.min(abs(brush$xmin - as.numeric(data$timestamp)))
  nearest_ecg[2] <- which.min(abs(brush$xmax - as.numeric(data$timestamp)))
  new_gap <- data.frame(timestamp_begin = data$timestamp[nearest_ecg[1]],
                        timestamp_end = data$timestamp[nearest_ecg[2]])

  # Add new_gap to gaps
  new_gaps <- rbind(gaps, new_gap) %>%
    dplyr::arrange(timestamp_begin)

  # Remove beats under gap
  new_beats <- beats
  if (!is.null(beats) && nrow(beats) > 0) {
    new_beats <- dplyr::filter(beats,
                               !dplyr::between(timestamp,
                                               new_gap$timestamp_begin,
                                               new_gap$timestamp_end))
  }

  # Merge overlapping gaps
  last_end <- max(c(new_gaps$timestamp_end, new_gaps$timestamp_end)) + 1
  overlapping <- which(new_gaps$timestamp_end > dplyr::lead(new_gaps$timestamp_begin))
  while (length(overlapping) > 0) {
    i <- overlapping[1]
    new_gaps$timestamp_end[i] <- new_gaps$timestamp_end[i + 1]
    new_gaps <- new_gaps[-(i + 1), ]
    overlapping <- which(new_gaps$timestamp_end > dplyr::lead(new_gaps$timestamp_begin))
  }

  list(heart_beats = new_beats,
       ecg_gaps = new_gaps)
}

# Clear a gap
clear_gap <- function(data, click, beats, gaps) {
  new_gaps <- gaps
  # Look for a gap under the click and remove it
  gap <- which(click$x > gaps$timestamp_begin & click$x < gaps$timestamp_end)
  if (length(gap) == 1)
    new_gaps <- dplyr::slice(gaps, -gap)

  list(heart_beats = beats,
       ecg_gaps = new_gaps)
}

# Set beat threshold
beat_thr <- function(data, click, beats, gaps) {
  # Look for peaks in 1s windows
  window_size <- 100
  local_peaks <- zoo::rollapply(data$ecg,
                                window_size,
                                function(x) which.max(x) == window_size / 2,
                                partial = TRUE)

  # Remove local peaks under threshold
  local_peaks[data$ecg < click$y] <- FALSE

  # Add new beats
  new_beats <- rbind(beats, data[local_peaks,]) %>%
    dplyr::arrange(timestamp) %>%
    dplyr::distinct()

  # Remove new beats if under gap
  # Remove beats under gap
  if (!is.null(gaps)) {
    visible_gaps <- dplyr::filter(gaps,
                                  timestamp_end >= min(data$timestamp),
                                  timestamp_begin <= max(data$timestamp))
    if (nrow(visible_gaps) > 0) {
      # utility function, checks if a timestamp falls in any visible gap
      is_under_gap <- function(t) {
        purrr::map_lgl(t, function(t) {
          any(t >= visible_gaps$timestamp_begin &
                t <= visible_gaps$timestamp_end)
        })
      }
      # data frame of beats under gaps
      gap_beats <- dplyr::mutate(new_beats,
                                 under_gap = is_under_gap(timestamp)) %>%
        dplyr::filter(under_gap)
      # anti join gap beats out of new beats
      new_beats <- dplyr::anti_join(new_beats, gap_beats, by = "timestamp")
    }
  }

  list(heart_beats = new_beats,
       ecg_gaps = gaps)
}

# Update plot data
subdata <- function(data, brush) {
  shiny::req(data, brush)
  dplyr::filter(data,
                timestamp >= brush$xmin,
                timestamp <= brush$xmax)
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
  # Find the heart beat before each gap and remove period/freq
  gap_beats <- unique(findInterval(gaps$timestamp_begin, result$timestamp))
  gap_beats <- gap_beats[gap_beats > 0]
  # Keep timestamp and ecg (cols 1 and 2) but set everything else to NA
  result[gap_beats, -(1:2)] <- NA

  # Return result
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
