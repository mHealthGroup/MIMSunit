#' Cut off input multi-channel signal according to a new dynamic range
#'
#' \code{cut_off_signal} cuts off the input multi-channel accelerometer data
#' according to a new dynamic range, then adds gausian noise to the cut-off
#' samples.
#'
#' This function simulates the behavior that a low dynamic range device is
#' trying to record high intensity movement, where recorded accelerometer signal
#' will be cut off at the dynamic range, but the true movement should have
#' higher acceleration values than the dynamic range. This function also adds
#' gaussian noise to the cut off samples to better simulate the real world
#' situation.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is a utility
#'   function that is used to simulate the behaviors of low dynamic range
#'   devices during algorithm validation.
#'
#' @param df dataframe. Input multi-channel accelerometer data.
#' @param range numerical vector. The new dynamic ranges to cut off the signal.
#'   Should be a 2-element numerical vector. \code{c(low, high)}, where
#'   \code{low} is the negative max value the device can reach and \code{high}
#'   is the positive max value the device can reach. Default is \code{NULL},
#'   meaning the function will do nothing but return the input data.
#' @param noise_std number. The standard deviation of the added gaussian noise.
#' @return dataframe. The multi-channel accelerometer data with the new dynamic
#'   range as specified in \code{range}.
#' @family utility functions
#' @export
#' @examples
#'   # Use sample data for testing
#'   df = sample_raw_accel_data
#'
#'   # Show df
#'   illustrate_signal(df, range=c(-8, 8))
#'
#'   # cut off the signal to c(-2, 2)
#'   new_df = cut_off_signal(df, range=c(-2, 2), noise_std=0.03)
#'
#'   # Show new df
#'   illustrate_signal(new_df, range=c(-2, 2))
cut_off_signal <- function(df,
                           range = NULL,
                           noise_std = 0.03) {
  if (!is.null(range)) {
    upper <- range[2]
    lower <- range[1]
    crop_fun <- plyr::colwise(function(col_data) {
      col_data[col_data > upper] <-
        upper + stats::rnorm(sum(col_data > upper), 0, noise_std)
      col_data[col_data < lower] <-
        lower + stats::rnorm(sum(col_data < lower), 0, noise_std)
      return(col_data)
    })
    df[, 2:ncol(df)] <- crop_fun(df[2:ncol(df)])
  }
  return(df)
}

#' Simulate new data based on the given multi-channel accelerometer data
#'
#' \code{simulate_new_data} simulate new data based on the given multi-channel
#' accelerometer data, a new dynamic range and a new sampling rate.
#'
#' This function simulates the data from a new device based on the signal from a
#' baseline device. It first changes the sampling rate using function
#' \code{\link{interpolate_signal}}, and then changes the dynamic range using
#' function \code{\link{cut_off_signal}}.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is a utility
#'   function that is used to simulate new devices with different sampling rates
#'   and dynamic ranges during algorithm validation.
#'
#' @param old_data dataframe. Input multi-channel accelerometer data.
#' @param new_range numerical vector. The new dynamic ranges to cut off the signal.
#'   Should be a 2-element numerical vector. \code{c(low, high)}, where
#'   \code{low} is the negative max value the device can reach and \code{high}
#'   is the positive max value the device can reach. Default is \code{NULL},
#'   meaning the function will do nothing but return the input data.
#' @param new_sr number. New sampling rate in Hz.
#' @family utility functions
#' @export
#' @examples
#'   # Use sample data for testing
#'   df = sample_raw_accel_data
#'
#'   # Show df
#'   illustrate_signal(df, range=c(-8, 8))
#'
#'   # simulate new data by changing range and sampling rate
#'   new_df = simulate_new_data(df, new_range=c(-2, 2), new_sr = 30)
#'
#'   # Show new df
#'   illustrate_signal(new_df, range=c(-2, 2))
simulate_new_data <- function(old_data, new_range, new_sr) {
  if (sampling_rate(old_data) != new_sr) {
    new_data <-
      interpolate_signal(old_data, method = "spline_natural", sr = new_sr)
  } else {
    new_data <- old_data
  }
  new_data <- cut_off_signal(new_data, range = new_range)
  return(new_data)
}
