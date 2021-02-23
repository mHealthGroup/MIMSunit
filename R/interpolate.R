#' Interpolate missing points and unify sampling rate for multi-channel signal
#'
#' \code{interpolate_signal} applies different interpolation algorithms to the
#' input multi-channel signal to fill in the missing samples and harmonizes the
#' sampling rate.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is a utility
#'   function that has been used in functions: \code{\link{extrapolate}}, and
#'   \code{\link{simulate_new_data}}.
#'
#' @param df dataframe. Input multi-channel accelerometer signal.
#' @param method string. Interpolation algorithms. Could be "spline_natural",
#'   "spline_improved" or "spline_fmm": see \code{\link[stats]{splinefun}};
#'   and "linear": see \code{\link[stats]{approxfun}}. Default is "spline_natural".
#' @param sr number. Sampling rate in Hz of the output signal. Default is 100.
#' @param st POSIXct date. The start time for interpolation. If it is
#'   \code{NULL}, it will use the start time of the input signal. Default is
#'   \code{NULL}.
#' @param et POSIXct date. The end time for interpolation. If it is \code{NULL},
#'   it will use the end time of the input signal. Default is \code{NULL}.
#' @return dataframe. Interpolated signal.
#' @family utility functions
#' @export
#' @examples
#'   # Use sample data
#'   df = sample_raw_accel_data
#'
#'   # Plot input
#'   illustrate_signal(df, plot_maxed_out_line=FALSE)
#'
#'   # Interpolate to 100 Hz
#'   sr = 100
#'
#'   # Interpolate the entire sequence of data
#'   output = interpolate_signal(df, sr=sr)
#'
#'   # Plot output
#'   illustrate_signal(output, plot_maxed_out_line=FALSE)
#'
#'   # Interpolate part of the sequence
#'   output = interpolate_signal(df, sr=sr, st=df[10,1], et=df[100,1])
#'
#'   # Plot output
#'   illustrate_signal(output, plot_maxed_out_line=FALSE)
interpolate_signal <-
  function(df,
           method = "spline_natural",
           sr = 100,
           st = NULL,
           et = NULL) {
    time_zone <- lubridate::tz(df[[1, 1]])
    n_rows <- nrow(df)
    if (is.null(st)) {
      st <- df[[1]][1]
    }
    if (is.null(et)) {
      et <- df[[1]][n_rows]
    }
    n_cols <- ncol(df)
    x_out <- seq(from = st, to = et, by = 1 / sr)
    ts <- df[[1]]
    values <- as.data.frame(df[2:n_cols])
    cn1 = colnames(df)[1]
    rm(df)
    result <- plyr::alply(values,
      .margins = 2, function(col_data) {
        col_name <- names(col_data)[1]
        col_data <- col_data[[1]]
        output <-
          switch(
            method,
            linear = stats::approx(x = ts, y = col_data, xout = x_out),
            spline_fmm = stats::spline(
              x = ts,
              y = col_data,
              xout = x_out,
              method = "fmm"
            ),
            spline_natural = stats::spline(
              x = ts,
              y = col_data,
              xout = x_out,
              method = "natural"
            ),
            spline_improved = stats::spline(
              x = ts,
              y = col_data,
              xout = x_out,
              method = "improved"
            )
          )
        output <- data.frame(output)
        colnames(output) <- c(cn1, col_name)
        return(output)
      }
    )
    rm(x_out)
    result <- Reduce(
      function(x, y) {
        return(dplyr::inner_join(x, y, by = colnames(x)[1]))
      },
      result
    )
    names(result[2:ncol(result)]) <-
      paste("INTERPOLATED", names(result[2:ncol(result)]), sep = "_")
    result[, 1] <-
      as.POSIXct(result[, 1], origin = "1970-01-01", tz = time_zone)
    result = as.data.frame(result)
    return(result)
  }
