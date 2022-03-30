#' Sample raw accelerometer data
#'
#' A raw accelerometer data file contains treadmill data collected from a human
#' subject.
#'
#' @format A data frame with 480 rows and 4 variables:
#' \describe{
#'   \item{HEADER_TIME_STAMP}{Timestamp, in POSIXct}
#'   \item{X}{X axis values, in number}
#'   \item{Y}{Y axis values, in number}
#'   \item{Z}{Z axis values, in number}
#' }
#' @source \url{https://github.com/mHealthGroup/MIMSunit/}
"sample_raw_accel_data"

#' Coefficient of variation values for different acceleration data summary
#' algorithms
#'
#' A dataset containing the coefficient of variation values at different
#' frequencies for the dataset that includes accelerometer measures of different
#' devices on a standard elliptical shaker.
#'
#' @format A data frame with 30 rows and 3 variables:
#' \describe{
#'   \item{TYPE}{Accelerometer summary algorithm name, in character}
#'   \item{HZ}{The frequency of the elliptical shaker, in number}
#'   \item{COEFF_OF_VARIATION}{The coefficient of variation values, in number}
#' }
#' @source \url{https://github.com/mHealthGroup/MIMSunit-dataset-shaker/}
"cv_different_algorithms"


#' The mean and standard deviation of accelerometer summary measure for
#' different acceleration data summary algorithms and for different devices.
#'
#' A dataframe contains the mean and standard deviation of accelerometer summary
#' measured at different frequencies for the raw accelerometer signals
#' from different devices collected from on a standard elliptical shaker.
#'
#' @format A data frame with 235 rows and 8 variables:
#' \describe{
#'   \item{DEVICE}{The name of different devices, in character}
#'   \item{GRANGE}{The dynamic range of the device in g, in number}
#'   \item{SR}{The sampling rate in Hz of the device, in number}
#'   \item{TYPE}{Accelerometer summary algorithm name, in character}
#'   \item{HZ}{The frequency of the elliptical shaker, in number}
#'   \item{NAME}{An alternative name that is friendly for plotting for devices, in character}
#'   \item{mean}{The mean values of accelerometer summary measure, in number}
#'   \item{sd}{The standard deviation values of accelerometer summary measure, in number}
#' }
#' @source \url{https://github.com/mHealthGroup/MIMSunit-dataset-shaker/}
"measurements_different_devices"


#' The input accelerometer data used to generate the conceptual diagram
#' (Figure 1) in the manuscript.
#'
#' The dataset includes accelerometer data from four devices. Device 0 is a real
#'  Actigraph GT9X device configured at 80Hz and 8g. Device 1 to 3 are simulated
#'  data from the data of device 0 using function \code{\link{simulate_new_data}}.
#'  Data for device 0 is a random selected nondominant wrist data from a
#'  participant doing Jumping jack. The data is manipulated to insert an
#'  artificial impulse to demonstrate the effect of the MIMS-unit algorithm when
#'  dealing on it.
#'
#' @format A data frame with 1704 rows and 5 variables:
#' \describe{
#'   \item{HEADER_TIME_STAMP}{The timestamp of raw accelerometer data, in POSIXct}
#'   \item{X}{The x axis value of raw accelerometer data, in number}
#'   \item{GRANGE}{The dynamic range of the simulated device in g, in number}
#'   \item{SR}{The sampling rate in Hz of the simulated device, in number}
#'   \item{NAME}{An alternative name that is friendly for plotting for different devices, in character}
#' }
#' @source \url{https://github.com/mHealthGroup/MIMSunit/}
"conceptual_diagram_data"

#' A short snippet of raw accelerometer signal from a device resting on a table.
#'
#' The dataset includes accelerometer data sampled at 80Hz and 6g. This data is used to derive the thresholding.
#'
#' @format A data frame with 5000 rows and 4 variables:
#' \describe{
#'   \item{HEADER_TIME_STAMP}{The timestamp of raw accelerometer data, in POSIXct}
#'   \item{X}{The x axis value of raw accelerometer data, in number}
#'   \item{Y}{The x axis value of raw accelerometer data, in number}
#'   \item{Z}{The x axis value of raw accelerometer data, in number}
#' }
#' @source \url{https://github.com/mHealthGroup/MIMSunit/}
"rest_on_table"

#' A short snippet of raw accelerometer signal from a device that has ending data maxed out.
#'
#' The dataset includes accelerometer data sampled at 80Hz and 6g. This data is used to test the edge case.
#'
#' @format A data frame with 20001 rows and 4 variables:
#' \describe{
#'   \item{HEADER_TIME_STAMP}{The timestamp of raw accelerometer data, in POSIXct}
#'   \item{X}{The x axis value of raw accelerometer data, in number}
#'   \item{Y}{The x axis value of raw accelerometer data, in number}
#'   \item{Z}{The x axis value of raw accelerometer data, in number}
#' }
#' @source \url{https://github.com/mHealthGroup/MIMSunit/}
"edge_case"
