#' ECG of a blue whale
#'
#' A dataset containing the ECG record of a blue whale, collected 2019-09-18
#' off the coast of Big Sur, CA under NMFS permit no. #####.
#'
#' @format A data frame with 234,001 rows and two variables:
#' \describe{
#'   \item{timestamp}{time of record, in POSIXct format}
#'   \item{ecg}{value from ECG recorder in engineering units, ranges 0-4095}
#' }
"ecg_bw190918_62R"

#' Heart rate of a blue whale
#'
#' A dataset containing the heart rate record of a blue whale, collected 2019-09-18
#' off the coast of Big Sur, CA under NMFS permit no. #####.
#'
#' @format A data frame with 59 rows and five variables:
#' \describe{
#'   \item{timestamp}{time of heart beat, in POSIXct format}
#'   \item{ecg}{value from ECG recorder at peak voltage (see \link{ecg_bw190918_62R})}
#'   \item{period_s}{period in seconds to next heart beat. NA if followed by a gap.}
#'   \item{freq_hz}{frequency of heart beat in Hz (inverse of period_s)}
#'   \item{freq_bpm}{frequency of heart beat in beats-per-minute}
#' }
"beats_bw190918_62R"
