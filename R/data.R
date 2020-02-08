#' ECG of a blue whale
#'
#' A dataset containing the ECG record of a blue whale, collected 2019-09-18
#' off the coast of Big Sur, CA under NMFS permit no. 16111.
#'
#' @format A data frame with 234,001 rows and two variables:
#' \describe{
#'   \item{timestamp}{time of record, in POSIXct format}
#'   \item{ecg}{value from ECG recorder in engineering units, ranges 0-4095}
#' }
#'
#' @references Goldbogen, J. A., Cade, D. E., Calambokidis, J., Czapanskiy, M. F., Fahlbusch, J., Friedlaender, A. S., … Ponganis, P. J. (2019). Extreme bradycardia and tachycardia in the world’s largest animal. *Proceedings of the National Academy of Sciences, 116*(50), 25329–25332. doi: 10.1073/pnas.1914273116
"ecg_bw190918_62R"

#' Heart rate of a blue whale
#'
#' A dataset containing the heart rate record of a blue whale, collected
#' 2019-09-18 off the coast of Big Sur, CA under NMFS permit no. 16111.
#'
#' @format A data frame with 59 rows and five variables:
#' \describe{
#'   \item{timestamp}{time of heart beat, in POSIXct format}
#'   \item{ecg}{value from ECG recorder at peak voltage (see \link{ecg_bw190918_62R})}
#'   \item{period_s}{period in seconds to next heart beat. NA if followed by a gap.}
#'   \item{freq_hz}{frequency of heart beat in Hz (inverse of period_s)}
#'   \item{freq_bpm}{frequency of heart beat in beats-per-minute}
#' }
#'
#' @references Goldbogen, J. A., Cade, D. E., Calambokidis, J., Czapanskiy, M. F., Fahlbusch, J., Friedlaender, A. S., … Ponganis, P. J. (2019). Extreme bradycardia and tachycardia in the world’s largest animal. *Proceedings of the National Academy of Sciences, 116*(50), 25329–25332. doi: 10.1073/pnas.1914273116
"beats_bw190918_62R"
