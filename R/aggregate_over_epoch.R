#' Aggregate over epoch to get numerically integrated values.
#'
#' \code{aggregate_for_mims} returns a dataframe with integrated values by
#' trapzoidal method over each epoch for each column. The epoch start time will
#' be used as timestamp in the first column.
#'
#' This function accepts a dataframe (in mhealth accelerometer data format) and
#' computes its aggregated values over each fixed epoch using different
#' integration methods (default is trapzoidal method, other methods are not used
#' by mims unit algorithm) for each value columns. The returned dataframe will
#' have the same number of columns as input dataframe, and have the same
#' datetime format as input dataframe in the timestamp column. The trapzoidal
#' method used in the function is based on \code{\link[caTools]{trapz}}.
#'
#' @section How is it used in mims-unit algorithm?: This function is used in
#'   mims-unit algorithm after filtering (\code{\link{iir}}). The filtered
#'   signal will be rectified and integrated to get mims unit values for each
#'   axis using this function.
#'
#' @param df dataframe of accelerometer data in mhealth format. First column
#'   should be timestamps in POSIXt format.
#' @param epoch string. Any format that is acceptable by argument \code{breaks}
#'   in method \code{\link[base]{cut.POSIXt}}.For example, "1 sec", "1 min", "5
#'   secs", "10 mins".
#' @param method string. Integration methods. Supported strings include:
#'   "trapz", "power", "sum", "meanBySecond", "meanBySize". Default is "trapz".
#' @param rectify logical. If TRUE, input data will be rectified before
#'   intregration. Default is TRUE.
#' @return dataframe. The returned dataframe will have the same format as input
#'   dataframe.
#'
#' @family aggregate functions
#' @seealso \code{\link{aggregate_for_orientation}} for aggregating to get
#'   accelerometer orientation estimation for each epoch.
#'
#' @note If \code{epoch} argument is not provided or is \code{NULL}, the
#'   function will treat the input dataframe as a single epoch.
#' @export
#'
aggregate_for_mims <-
  function(df,
           epoch,
           method = "trapz",
           rectify = TRUE)
  {
    time_zone <- lubridate::tz(df[1, 1])
    n_cols <- ncol(df)

    # parse input argument epoch
    if (missing(epoch) || is.null(epoch))
    {
      df$SEGMENT <- 1
    } else
    {
      df <- mHealthR::mhealth.segment(df, epoch, file_type = "sensor")
    }

    # get the number of samples in each epoch
    n_threshold <-
      break_str_to_sample_size(ts = df[, 1],
                               breaks = epoch,
                               sr = sampling_rate(df))

    # iterate over each value column
    result <- plyr::ddply(df, c("SEGMENT"), function(rows)
    {
      rows[, 1] <- as.numeric(rows[, 1])
      rows <- stats::na.omit(rows)

      # do integration if there are enough (90%) number of samples
      if (nrow(rows) >= 0.9 * n_threshold)
      {
        # do rectification if rectify is TRUE
        if (rectify)
        {
          rows[2:n_cols] <- (plyr::numcolwise(function(col_data)
          {
            col_data[col_data > -150] <- abs(col_data[col_data > -150])
            if (any(col_data < 0))
            {
              col_data <- rep(-200, length(col_data))
            }
            return(col_data)
          }))(rows[2:n_cols])
        }

        # select different methods for integration
        if (method == "trapz")
        {
          auc_values <-
            (plyr::numcolwise(caTools::trapz, x = rows[, 1]))(rows[2:n_cols])
          max_values <- 16 * n_threshold
        } else if (method == "power")
        {
          auc_values <-
            (plyr::numcolwise(caTools::trapz,
                              x = rows[, 1]))(as.data.frame(rows[2:n_cols] ^ 2))
          max_values <- 16 ^ 2 * n_threshold
        } else if (method == "mean_by_time")
        {
          auc_values <-
            (plyr::numcolwise(sum))(rows[2:n_cols]) /
            (max(rows[, 1]) - min(rows[, 1]))
          max_values <- 16 * n_threshold / 32
        } else if (method == "mean_by_size")
        {
          auc_values <-
            (plyr::numcolwise(sum))(rows[2:n_cols]) / length(rows[, 1])
          max_values <- 16
        } else if (method == "sum")
        {
          auc_values <- (plyr::numcolwise(sum))(rows[2:n_cols])
          max_values <- 16 * nrow(rows)
        }
      } else
      {
        auc_values <- as.data.frame(lapply(rows, function(x)
          rep.int(-1, 1)))
        auc_values <- auc_values[2:n_cols]
        max_values <- 0
      }

      # flag extra large (abnormal) values
      auc_values[auc_values >= max_values] <- -1
      auc_values[auc_values < 0] <- -1
      return(data.frame(ts = rows[1, 1], auc_values))
    })

    # format output
    result$SEGMENT <- NULL
    names(result)[1] <- names(df)[1]
    for (i in 2:n_cols)
    {
      names(result)[i] <- paste("AGGREGATED", names(result)[i], sep = "_")
    }
    result[1] <-
      as.POSIXct(result[[1]], origin = "1970-01-01", tz = time_zone)
    return(result)
  }

#' Aggregate over epoch to get estimated accelerometer orientation.
#'
#' \code{aggregate_for_orientation} returns a dataframe with accelerometer
#' orientations estimated by
#' \href{https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=1241424}{Mizell,
#' 2003} over each epoch. The epoch start time will be used as timestamp in the
#' first column.
#'
#' This function accepts a dataframe (in mhealth accelerometer data format) and
#' computes the estimated acclerometer orientations (in x, y, and z angles) over
#' each fixed epoch. The returned dataframe will have the same format as input
#' dataframe, including four columns, and have the same datetime format as input
#' dataframe in the timestamp column. The orientation estimation method used in
#' the function is based on
#' \href{https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=1241424}{Mizell,
#' 2003}.
#'
#' @section How is it used in mims-unit algorithm?: This function is used in
#'   mims-unit algorithm after extrapolation (\code{\link{extrapolate}}). The
#'   extrapolated signal will be estimated to get orientation angles using this
#'   function.
#'
#' @param df dataframe of accelerometer data in mhealth format. First column
#'   should be timestamps in POSIXt format.
#' @param epoch string. Any format that is acceptable by argument \code{breaks}
#'   in method \code{\link[base]{cut.POSIXt}}.For example, "1 sec", "1 min", "5
#'   secs", "10 mins".
#' @param estimation_duration numeric value. duration in seconds to be used to
#'   estimate orientation within each epoch. Default is 2 (seconds), as
#'   suggested by
#'   \href{https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=1241424}{Mizell,
#'   2003}.
#' @param unit string. The unit of orientation angles. Can be "deg" (degree) or
#'   "rad" (radian). Default is "deg".
#' @return dataframe. The returned dataframe will have the same format as input
#'   dataframe.
#'
#' @family aggregate functions
#' @seealso \code{\link{aggregate_for_mims}} for aggregating to get integrated
#'   values for each axis for each epoch.
#'
#' @note If \code{epoch} argument is not provided or is \code{NULL}, the
#'   function will treat the input dataframe as a single epoch.
#' @export
#'
aggregate_for_orientation <-
  function(df,
           epoch,
           estimation_duration = 2,
           unit = "deg")
  {
    time_zone <- lubridate::tz(df[1, 1])
    n_cols <- ncol(df)

    # parse input argument epoch
    if (missing(epoch) || is.null(epoch))
    {
      df$SEGMENT <- 1
    } else
    {
      df <- mHealthR::mhealth.segment(df, epoch, file_type = "sensor")
    }

    # get the desired number of samples in each epoch
    n_threshold <-
      break_str_to_sample_size(ts = df[, 1],
                               breaks = epoch,
                               sr = sampling_rate(df))

    # iterate over each column
    result <- plyr::ddply(df, c("SEGMENT"), function(rows)
    {
      rows <- stats::na.omit(rows)

      # Do estimation if there are enough samples in the epoch
      if (nrow(rows) >= 0.9 * n_threshold)
      {
        ori_values <-
          compute_orientation(rows[, 1:n_cols],
                              epoch = estimation_duration,
                              unit = unit)
      } else
      {
        ori_values <-
          data.frame(
            HEADER_TIME_STAMP = rows[1, 1],
            X_ANGLE = NaN,
            Y_ANGLE = NaN,
            Z_ANGLE = NaN
          )
      }
      return(ori_values)
    })

    # format output
    result$SEGMENT <- NULL
    names(result)[1] <- names(df)[1]
    result[1] <-
      as.POSIXct(result[[1]], origin = "1970-01-01", tz = time_zone)
    return(result)
  }
