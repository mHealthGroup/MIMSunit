#' Remove constant component of the signal
#'
#' \code{remove_average} function takes a multi-channel signal and removes the
#' average value over a filtering window.
#'
#' This function filters the input multi-channel signal by removing the average
#' value within each sliding window. The sliding window size is decided by
#' \eqn{w = sr * order}.
#'
#' @section How is it used in MIMS-unit algorithm?: This function has been
#'   considered as one of filtering options during the development of MIMS-unit
#'   algorithm. But the released version of MIMS-unit algorithm does not use
#'   this function for filtering.
#'
#' @param df dataframe. The input multi-channel signal. The first column is
#'   timestamps in POSXlct format. The rest columns are signal values.
#' @param sr number. Sampling rate in Hz of the input signal.
#' @param order number. Window size (in seconds) of the filter. Default is 500
#'   ms.
#' @return dataframe. Filtered signal.
#' @family filtering functions
#' @export
#'
remove_average <- function(df, sr, order = 0.5) {
  n_cols <- ncol(df)

  window <- round(order * sr)

  b <-
    c((window - 1) / window, -matlab::ones(1, window - 1) / window)
  a <- 1

  col_filter <- plyr::colwise(
    .fun = function(x, filt, a) {
      filtered <- signal::filter(filt, a, x)
      result <- as.numeric(filtered)
      return(result)
    },
    filt = b,
    a = a
  )

  filtered_value <- col_filter(df[2:n_cols])
  colnames(filtered_value) <-
    paste0("AVERAGEREMOVAL_", colnames(filtered_value))
  filtered_value <- cbind(df[1], filtered_value)
  return(filtered_value)
}

#' Apply Bessel lowpass filter to the signal
#'
#' \code{bessel} function takes a multi-channel signal and applies a bessel
#' lowpass filter to the signal.
#'
#' This function filters the input multi-channel signal by appling a bessel
#' lowpass filter. See \href{https://en.wikipedia.org/wiki/Bessel_filter}{wiki}
#' for the explanation of the filter. The filter was implemented with the same
#' implementation as in MATLAB.
#'
#' @section How is it used in MIMS-unit algorithm?: This function has been
#'   considered as one of filtering options during the development of MIMS-unit
#'   algorithm. But the released version of MIMS-unit algorithm does not use
#'   this function for filtering.
#'
#' @param df dataframe. The input multi-channel signal. The first column is
#'   timestamps in POSXlct format. The rest columns are signal values.
#' @param sr number. Sampling rate in Hz of the input signal.
#' @param cutoff_freq number. The lowpass cutoff frequency in Hz.
#' @param order number. The order of the filter. Default is 8.
#' @return dataframe. Filtered signal.
#' @family filtering functions
#' @export
bessel <- function(df, sr, cutoff_freq, order = 8) {
  # real bessel filter design based on the implementation of matlab
  arma_coeffs <-
    .besself(
      sr = sr,
      cutoff_freq = cutoff_freq,
      order = order
    )

  n_cols <- ncol(df)

  col_filter <- plyr::colwise(
    .fun = function(x, filt) {
      filtered <- signal::filter(filt, x)
      return(as.numeric(filtered))
    },
    filt = arma_coeffs
  )
  filtered_value <- col_filter(df[2:n_cols])
  colnames(filtered_value) <-
    paste0("BESSEL_", colnames(filtered_value))
  filtered_value <- cbind(df[1], filtered_value)

  return(filtered_value)
}

#' Apply IIR filter to the signal
#'
#' \code{iir} function takes a multi-channel signal and applies an IIR filter to
#' the signal.
#'
#' This function filters the input multi-channel signal by appling an IIR
#' filter. See
#' \href{https://en.wikipedia.org/wiki/Infinite_impulse_response}{wiki} for the
#' explanation of the filter. The implementations of IIR filters can be found in
#' \code{\link[signal]{butter}}, \code{\link[signal]{cheby1}},
#' \code{\link[signal]{cheby2}}, and \code{\link[signal]{ellip}}.
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
#'   numerical vector specifing the low and high end cutoff frequencies
#'   \code{c(low, high)}.
#' @param order number. The order of the filter. Default is 4.
#' @param type string. Filtering type, one of "low" for a low-pass filter,
#'   "high" for a high-pass filter, "stop" for a stop-band (band-reject) filter,
#'   or "pass" for a pass-band filter.
#' @param filter_type string. IIR filter type, one of "butter" for butterworth
#'   filter, "chebyI" for Chebyshev Type I filter, "chebyII" for Chebyshev Type
#'   II filter, or "ellip" for Elliptic filter.
#' @return dataframe. Filtered signal.
#' @family filtering functions
#' @export
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

    col_filter <- plyr::colwise(
      .fun = function(x, filt, a) {
        filtered <- signal::filter(filt, a, x)
        result <- as.numeric(filtered)
        return(result)
      },
      filt = coeffs$b,
      a = coeffs$a
    )
    filtered_value <- col_filter(df[2:n_cols])
    colnames(filtered_value) <-
      paste0("IIR_", colnames(filtered_value))
    filtered_value <- cbind(df[1], filtered_value)
    return(filtered_value)
  }

#' Apply a bandlimited interpolation filter to the signal to change the sampling
#' rate
#'
#' \code{bandlimited_interp} function takes a multi-channel signal and applies a
#' bandlimited interpolation filter to the signal to change its sampling rate.
#'
#' This function filters the input multi-channel signal by appling a bandlimited
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
#'
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


.besselap <- function(n) {
  z <- c()
  k <- 1
  if (n == 0) {
    p <- c()
  } else if (n == 1) {
    p <- c(-1)
  } else if (n == 2) {
    p <-
      c(
        complex(real = -0.866025403784439, imaginary = 0.5),
        complex(
          real = -0.866025403784439,
          imaginary = -0.5
        )
      )
  } else if (n == 3) {
    p <-
      c(
        -0.941600026533207,
        complex(
          real = -0.745640385848077,
          imaginary = -0.711366624972835
        ),
        complex(real = -0.745640385848077, imaginary = 0.711366624972835)
      )
  } else if (n == 4) {
    p <-
      c(
        complex(
          real = -0.657211171671883,
          imaginary = -0.830161435004873
        ),
        complex(real = -0.657211171671883, imaginary = 0.830161435004873),
        complex(
          real = -0.904758796788245,
          imaginary = -0.270918733003875
        ),
        complex(real = -0.904758796788245, imaginary = 0.270918733003875)
      )
  } else if (n == 5) {
    p <-
      c(
        -0.92644207738776,
        complex(
          real = -0.85155361936884,
          imaginary = -0.442717463944333
        ),
        complex(real = -0.85155361936884, imaginary = 0.442717463944333),
        complex(
          real = -0.590575944611919,
          imaginary = -0.907206756457455
        ),
        complex(real = -0.590575944611919, imaginary = 0.907206756457455)
      )
  } else if (n == 6) {
    p <-
      c(
        complex(
          real = -0.909390683047227,
          imaginary = -0.185696439679305
        ),
        complex(real = -0.909390683047227, imaginary = 0.185696439679305),
        complex(
          real = -0.799654185832829,
          imaginary = -0.562171734693732
        ),
        complex(real = -0.799654185832829, imaginary = 0.562171734693732),
        complex(
          real = -0.538552681669311,
          imaginary = -0.961687688195428
        ),
        complex(real = -0.538552681669311, imaginary = 0.961687688195428)
      )
  } else if (n == 7) {
    p <-
      c(
        -0.919487155649029,
        complex(
          real = -0.880002934152337,
          imaginary = -0.321665276230774
        ),
        complex(real = -0.880002934152337, imaginary = 0.321665276230774),
        complex(
          real = -0.752735543409321,
          imaginary = -0.650469630552255
        ),
        complex(real = -0.752735543409321, imaginary = 0.650469630552255),
        complex(
          real = -0.496691725667232,
          imaginary = -1.00250850845442
        ),
        complex(real = -0.496691725667232, imaginary = 1.00250850845442)
      )
  } else if (n == 8) {
    p <-
      c(
        complex(
          real = -0.909683154665291,
          imaginary = -0.141243797667142
        ),
        complex(real = -0.909683154665291, imaginary = 0.141243797667142),
        complex(
          real = -0.847325080235933,
          imaginary = -0.425901753827294
        ),
        complex(real = -0.847325080235933, imaginary = 0.425901753827294),
        complex(
          real = -0.71113818084854,
          imaginary = -0.71865173141084
        ),
        complex(real = -0.71113818084854, imaginary = 0.71865173141084),
        complex(
          real = -0.462174041253212,
          imaginary = -1.0343886811269
        ),
        complex(real = -0.462174041253212, imaginary = 1.0343886811269)
      )
  }
  return(list(z = z, p = p, k = k))
}

.bilinear <- function(z, p, k, sr) {
  sr <- 2 * sr
  pd <- (1 + p / sr) / (1 - p / sr)
  zd <- (1 + z / sr) / (1 - z / sr)
  kd <- k * prod(sr - z) / prod(sr - p)
  zd <- c(zd, -rep(1, length(pd) - length(zd)))
  return(list(zd = zd, pd = pd, kd = kd))
}

.besself <- function(sr, cutoff_freq, order) {
  zpk_prototype <- .besselap(order)
  zpk_prototype <-
    signal::sftrans(signal::Zpg(
      zpk_prototype$z,
      zpk_prototype$p,
      zpk_prototype$k
    ),
    W = cutoff_freq * 2 * pi
    )
  zpk_prototype <-
    .bilinear(
      zpk_prototype$zero,
      zpk_prototype$pole,
      zpk_prototype$gain,
      sr
    )
  arma_coeffs <-
    signal::as.Arma(signal::Zpg(
      zpk_prototype$z,
      zpk_prototype$p,
      zpk_prototype$k
    ))
  return(arma_coeffs)
}
