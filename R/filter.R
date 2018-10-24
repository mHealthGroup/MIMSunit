#' @name average_removal
#' @title Apply average removal FIR filter to the input sensor data frame each column over certain breaks (e.g. hour, sec, min and etc.)
#' @export
#' @importFrom signal filter
#' @importFrom plyr colwise
#' @importFrom matlab ones
#' @param df the input dataframe that matches mhealth specification.
#' @param sr sampling rate of the input signal
#' @param order window size (in seconds) of filter
average_removal <- function(df, sr, order)
{
  n_cols <- ncol(df)

  window <- round(order * sr)

  b <-
    c((window - 1) / window, -matlab::ones(1, window - 1) / window)
  a <- 1

  col_filter <- plyr::colwise(
    .fun = function(x, filt, a)
    {
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

#' @name bessel
#' @title Apply low pass bessel filter to the input sensor data frame each column over certain breaks (e.g. hour, sec, min and etc.)
#' @export
#' @importFrom signal filter
#' @importFrom plyr colwise
#' @param df the input dataframe that matches mhealth specification.
#' @param sr sampling rate of the input signal
#' @param cutoff_freq cut off frequency of bessel filter
#' @param order formula order of bessel filter
bessel <- function(df, sr, cutoff_freq, order)
{
  # real bessel filter design based on the implementation of matlab
  arma_coeffs <-
    .besself(sr = sr,
             cutoff_freq = cutoff_freq,
             order = order)

  n_cols <- ncol(df)

  col_filter <- plyr::colwise(
    .fun = function(x, filt)
    {
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

#' @name iir
#' @title Apply iir filter to the input sensor data frame each column over a certain break (e.g. hour, sec, min and etc.).
#' @export
#' @importFrom signal butter cheby1 cheby2 ellip filter
#' @importFrom plyr colwise
#' @param df the input dataframe that matches mhealth specification.
#' @param sr sampling rate of the input signal
#' @param cutoff_freq cut off frequencies of butterworth filter, if more than one store as c(low, high)
#' @param order formula order of butterworth filter
#' @param type 'low', 'high', 'stop', 'pass'
#' @param filter_type 'butter', 'chebyI', 'chebyII', 'ellip'
#' @return list of filtered dataframes.
iir <-
  function(df,
           sr,
           cutoff_freq,
           order,
           type = "high",
           filter_type = "butter")
  {
    nyquist <- sr / 2

    coeffs <-
      switch(
        filter_type,
        butter = signal::butter(order, cutoff_freq / nyquist, type),
        chebyI = signal::cheby1(order, 0.05, W = cutoff_freq / nyquist,
                                type, plane = "z"),
        chebyII = signal::cheby2(order, 0.05, W = cutoff_freq / nyquist,
                                 type, plane = "z"),
        ellip = signal::ellip(order, 0.05,
                              50, W = cutoff_freq / nyquist, type, plane = "z")
      )

    n_cols <- ncol(df)

    col_filter <- plyr::colwise(
      .fun = function(x, filt, a)
      {
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

#' @name change_sampling_rate
#' @title Apply bandlimited interpolation filter to the input sensor data frame each column over a certain break (e.g. hour, sec, min and etc.).
#' @export
#' @importFrom signal resample
#' @importFrom plyr colwise
#' @importFrom magrittr %>%
#' @importFrom dplyr last
#' @param dft dataframe that matches mhealth specification.
#' @param orig_sr original sampling rate of each column
#' @param new_sr the desired sampling rate for each column
#' @return list of filtered dataframes.
change_sampling_rate <- function(df, orig_sr, new_sr)
{
  n_cols <- ncol(df)

  col_filter <- plyr::colwise(
    .fun = function(x)
    {
      resampled <-
        x %>% signal::resample(p = new_sr, q = orig_sr) %>% as.numeric()
    }
  )
  resampled_value <- col_filter(df[2:n_cols])
  resampled_ts <-
    seq(
      from = df[1, 1],
      to = df[, 1] %>% dplyr::last(),
      length = nrow(resampled_value)
    )
  resampled_value <- cbind(resampled_ts, resampled_value)
  colnames(resampled_value)[1] <- colnames(df)[1]
  return(resampled_value)
}


.besselap <- function(n)
{
  z <- c()
  k <- 1
  if (n == 0)
  {
    p <- c()
  } else if (n == 1)
  {
    p <- c(-1)
  } else if (n == 2)
  {
    p <-
      c(
        complex(real = -0.866025403784439, imaginary = 0.5),
        complex(
          real = -0.866025403784439,
          imaginary = -0.5
        )
      )
  } else if (n == 3)
  {
    p <-
      c(
        -0.941600026533207,
        complex(
          real = -0.745640385848077,
          imaginary = -0.711366624972835
        ),
        complex(real = -0.745640385848077, imaginary = 0.711366624972835)
      )
  } else if (n == 4)
  {
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
  } else if (n == 5)
  {
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
  } else if (n == 6)
  {
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
  } else if (n == 7)
  {
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
  } else if (n == 8)
  {
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

.bilinear <- function(z, p, k, sr)
{
  sr <- 2 * sr
  pd <- (1 + p / sr) / (1 - p / sr)
  zd <- (1 + z / sr) / (1 - z / sr)
  kd <- k * prod(sr - z) / prod(sr - p)
  zd <- c(zd, -rep(1, length(pd) - length(zd)))
  return(list(zd = zd, pd = pd, kd = kd))
}

#' @importFrom signal sftrans as.Arma Zpg
.besself <- function(sr, cutoff_freq, order)
{
  zpk_prototype <- .besselap(order)
  zpk_prototype <-
    signal::sftrans(signal::Zpg(zpk_prototype$z,
                                zpk_prototype$p,
                                zpk_prototype$k),
                    W = cutoff_freq * 2 * pi)
  zpk_prototype <-
    .bilinear(zpk_prototype$zero,
              zpk_prototype$pole,
              zpk_prototype$gain,
              sr)
  arma_coeffs <-
    signal::as.Arma(signal::Zpg(zpk_prototype$z,
                                zpk_prototype$p,
                                zpk_prototype$k))
  return(arma_coeffs)
}
