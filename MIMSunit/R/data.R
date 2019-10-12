#' Coefficient of variation values for different acceleration data summary
#' algorithms
#'
#' A dataset containing the coefficient of variation values at different
#' frequencies for the dataset that includes accelerometer measures of different
#' devices on a standard ellpitical shaker.
#'
#' @format A data frame with 30 rows and 3 variables:
#' \describe{
#'   \item{TYPE}{Accelerometer summary algorithm name, in character}
#'   \item{HZ}{The frequency of the elliptical shaker, in number}
#'   \item{COEFF_OF_VARIATION}{The coefficient of variation values, in number}
#' }
#' @source \url{https://github.com/qutang/MIMSunit-dataset-shaker}
"cv_different_algorithms"


#' The mean and standard deviation of accelerometer summary measures for
#' different acceleration data summary algorithms and for different devices.
#'
#' A dataset containing the mean and standard deviation of acceleometer summary
#' measures at different frequencies for the dataset that includes accelerometer
#' measures of different devices on a standard ellpitical shaker.
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
#' @source \url{https://github.com/qutang/MIMSunit-dataset-shaker}
"measurements_different_devices"