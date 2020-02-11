#' Read data from a UBE file
#'
#' \code{read_ube} parses a binary UBE file, returning a data frame with ECG
#' records and timestamps
#'
#' @param ube_path Path to UBE file.
#' @return A data.frame with columns timestamp (POSIXct) and ecg (numeric)
#' @examples
#' # File path to sample data
#' fp <- system.file("extdata", "max_ecg_190826b.ube", package = "beats")
#' # Read ube file
#' ecg_data <- read_ube(fp)
#' @export
read_ube <- function(ube_path) {
  # Read the raw data
  ube_raw <- readr::read_file_raw(ube_path)

  # First 32 bytes are the download timestamp
  dl_time <- rawToChar(ube_raw[1:32], multiple = TRUE) %>%
    paste(collapse = "") %>%
    lubridate::mdy_hms(tz = "UTC")

  # Next 5 bytes are the time of recording beginning
  # Not sure what bytes 38-40 are
  # Year doesn't seem to be recorded, so assume year is the same as now
  mdhms <- as.numeric(ube_raw[33:37])
  record_start <- ISOdatetime(lubridate::year(lubridate::now()), # year
                              mdhms[1],                          # month
                              mdhms[2],                          # day
                              mdhms[3],                          # hour
                              mdhms[4],                          # minute
                              mdhms[5],                          # second
                              tz = "UTC")

  # Remainder is data
  # Each record is 2 bytes, of the format 0xCddd where C is the channel (2 for
  # ECG) and ddd is the value

  # Remove the header
  data_raw <- ube_raw[-(1:40)]
  # ecg_channel is a mask for checking if a record is ECG data
  ecg_channel <- 0x2F
  mode(ecg_channel) <- "raw"
  # Since each record is two bytes, the output is at largest half the length of
  # the raw data
  ecg_data <- integer(length(data_raw) / 2)

  i <- 1
  for (raw_i in seq(1, length(data_raw), by = 2)) {
    # Check if a record is ecg data (i.e. starts with 0x2), otherwise skip it
    if ((ecg_channel | data_raw[raw_i]) != ecg_channel)
      next
    # This math gets the value of a nibble plus a byte
    num <- as.numeric(data_raw[raw_i:(raw_i+1)])
    ecg_data[i] <- (num[1] - 0x20) * 0x100 + num[2]
    i <- i + 1
  }

  # Trim the data to valid values
  ecg_data <- ecg_data[1:(i - 1)]

  # The timestamp of the ECG values assumes 100 Hz
  # In the future, the sampling rate could be read from the header
  ecg_time <- record_start + (seq_along(ecg_data) - 1) / 100

  # Create a result table and set an attribute for the creation time.
  result <- data.frame(timestamp = ecg_time,
                       ecg = ecg_data)
  attr(result, "created") <- dl_time
  result
}
