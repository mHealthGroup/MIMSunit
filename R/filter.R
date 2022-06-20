#' Apply IIR filter to the signal
#'
#' \code{iir} function takes a multi-channel signal and applies an IIR filter to
#' the signal.
#'
#' This function filters the input multi-channel signal by applying an IIR
#' filter. See
#' \href{https://en.wikipedia.org/wiki/Infinite_impulse_response}{wiki} for the
#' explanation of the filter. The implementations of IIR filters can be found in
#' \code{\link[signal]{butter}}, \code{\link[signal]{cheby1}}, and \code{\link[signal]{ellip}}.
#'
#' For Chebyshev Type I, Type II and Elliptic filter, the passband ripple is
#' fixed to be 0.05 dB. For Elliptic filter, the stopband ripple is fixed to be
#' -50dB.
#'
#' @section How is it used in MIMS-unit algorithm?: This function has been used
#'   as the main filtering method in MIMS-unit algorithm. Specifically, it uses
#'   a 0.5 - 5 Hz bandpass butterworth filter during filtering.
#'
#' @param df dataframe. The input multi-channel signal. The first column is
#'   timestamps in POSXlct format. The rest columns are signal values.
#' @param sr number. Sampling rate in Hz of the input signal.
#' @param cutoff_freq number or numerical vector. The cutoff frequencies in Hz.
#'   If the IIR filter is a bandpass or bandstop filter, it will be a 2-element
#'   numerical vector specifying the low and high end cutoff frequencies
#'   \code{c(low, high)}.
#' @param order number. The order of the filter. Default is 4.
#' @param type string. Filtering type, one of "low" for a low-pass filter,
#'   "high" for a high-pass filter, "stop" for a stop-band (band-reject) filter,
#'   or "pass" for a pass-band filter.
#' @param filter_type string. IIR filter type, one of "butter" for butterworth
#'   filter, "chebyI" for Chebyshev Type I filter, or "ellip" for Elliptic filter.
#' @return dataframe. Filtered signal.
#' @family filtering functions
#' @export
#' @examples
#'   # Use sample data
#'   df = sample_raw_accel_data
#'
#'   # View input
#'   illustrate_signal(df, plot_maxed_out_line = FALSE)
#'
#'   # Apply filtering that uses the same setting as in MIMSunit algorithm
#'   output = iir(df, sr=80, cutoff_freq=c(0.2, 5), type='pass')
#'
#'   # View output
#'   illustrate_signal(output, plot_maxed_out_line = FALSE)
iir <-
  function(df,
           sr,
           cutoff_freq,
           order = 4,
           type = "high",
           filter_type = "butter") {
    nyquist <- sr / 2

    coeffs <-
      switch(
        filter_type,
        butter = signal::butter(order, cutoff_freq / nyquist, type),
        chebyI = signal::cheby1(order, 0.05,
                                W = cutoff_freq / nyquist,
                                type, plane = "z"
        ),
        chebyII = signal::cheby2(order, 0.05,
                                 W = cutoff_freq / nyquist,
                                 type, plane = "z"
        ),
        ellip = signal::ellip(order, 0.05,
                              50,
                              W = cutoff_freq / nyquist, type, plane = "z"
        )
      )

    n_cols <- ncol(df)

    filt_fun = function(x, filt, a) {
      filtered <- signal::filter(filt, a, x)
      result <- as.numeric(filtered)
      return(result)
    }
    for (icol in 2:n_cols) {
      filt_result = filt_fun(
        df[[icol]],
        filt = coeffs$b,
        a = coeffs$a)
      df[[icol]] = filt_result
    }
    colnames(df)[2:n_cols] <-
      paste0("IIR_", colnames(df)[2:n_cols])
    return(df)
  }

#' Apply a bandlimited interpolation filter to the signal to change the sampling
#' rate
#'
#' \code{bandlimited_interp} function takes a multi-channel signal and applies a
#' bandlimited interpolation filter to the signal to change its sampling rate.
#'
#' This function filters the input multi-channel signal by applying a bandlimited
#' interpolation filter. See \code{\link[signal]{resample}} for the underlying
#' implementation.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is not used in
#'   the released version of MIMS-unit algorithm, but has once been considered
#'   to be used after extrapolation to harmonize sampling rate before filtering.
#'   But in the end, we decided to use linear interpolation before extrapolation
#'   to increase the sampling rate to 100Hz, so this method is no longer needed.
#'
#' @param df dataframe. The input multi-channel signal. The first column is
#'   timestamps in POSXlct format. The rest columns are signal values.
#' @param orig_sr number. Sampling rate in Hz of the input signal.
#' @param new_sr number. The desired sampling rate in Hz of the output signal.
#' @return dataframe. Filtered signal.
#' @family filtering functions
#' @export
#' @examples
#'   # Use sample data
#'   df = sample_raw_accel_data
#'
#'   # View input
#'   illustrate_signal(df, plot_maxed_out_line = FALSE)
#'
#'   # Apply filtering that uses the same setting as in MIMSunit algorithm
#'   output = bandlimited_interp(df, orig_sr=80, new_sr=30)
#'
#'   # View output
#'   illustrate_signal(output, plot_maxed_out_line = FALSE)
bandlimited_interp <- function(df, orig_sr, new_sr) {
  n_cols <- ncol(df)

  col_filter <- plyr::colwise(
    .fun = function(x) {
      resampled <-
        as.numeric(signal::resample(x, p = new_sr, q = orig_sr))
    }
  )
  resampled_value <- col_filter(df[2:n_cols])
  resampled_ts <-
    seq(
      from = df[1, 1],
      to = dplyr::last(df[, 1]),
      length = nrow(resampled_value)
    )
  resampled_value <- cbind(resampled_ts, resampled_value)
  colnames(resampled_value)[1] <- colnames(df)[1]
  return(resampled_value)
}

