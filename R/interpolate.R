#' @name interpolate_signal
#' @title Interpolate the missing points and unify sampling interval for the input sensor data
#' @export
interpolate_signal <-
  function(df,
           method = "spline_natural",
           sr = 100,
           st = NULL,
           et = NULL)
  {
    time_zone <- lubridate::tz(df[1, 1])
    n_rows <- nrow(df)
    if (is.null(st))
    {
      st <- df[[1]][1]
    }
    if (is.null(et))
    {
      et <- df[[1]][n_rows]
    }
    n_cols <- ncol(df)
    x_out <- seq(from = st, to = et, by = 1 / sr)
    ts <- df[, 1]
    values <- df[2:n_cols]
    result <- plyr::alply(values, .margins = 2, function(col_data)
    {
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
          aspline_original = akima::aspline(
            x = ts,
            y = col_data,
            xout = x_out,
            method = "original"
          ),
          aspline_improved = stats::spline(
            x = ts,
            y = col_data,
            xout = x_out,
            method = "improved"
          )
        )
      output <- data.frame(output)
      colnames(output) <- c(colnames(df)[1], col_name)
      return(output)
    },
    .progress = plyr::progress_text())
    result <- Reduce(function(x, y)
    {
      return(merge(x, y, by = colnames(x)[1]))
    },
    result)
    names(result[2:ncol(result)]) <-
      paste("INTERPOLATED", names(result[2:ncol(result)]), sep = "_")
    result[, 1] <-
      as.POSIXct(result[, 1], origin = "1970-01-01", tz = time_zone)
    return(result)
  }
