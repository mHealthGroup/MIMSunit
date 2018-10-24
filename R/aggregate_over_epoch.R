#' @name aggregate_for_mims
#' @title Calculate summary value (area under curve) for each column over a certain break (e.g. hour, min).
#' @note If certain break is not provided or missing, will use the entire sequence. The column name (except for the first column) of output dataframe would be: [SUMMARY\_METHOD]\_INPUT\_HEADER\_NAME.
#' @export
#' @importFrom plyr ddply numcolwise
#' @importFrom caTools trapz
#' @importFrom mHealthR mhealth.segment
#' @importFrom lubridate tz
#' @importFrom stats na.omit
#' @param df input dataframe that matches mhealth sensor data format.
#' @param breaks could be 'sec', 'min', 'hour', 'day', 'week', 'month', 'quarter' or 'year'; or preceded by an interger and a space.
#' @param type 'trapz', 'power', 'sum', 'meanBySecond', 'meanBySize'
#' @param rectify whether rectify the values before computing AUC, default is TRUE
aggregate_for_mims <-
  function(df,
           breaks,
           type = "trapz",
           rectify = TRUE)
  {
    time_zone <- lubridate::tz(df[1, 1])
    n_cols <- ncol(df)
    if (missing(breaks) || is.null(breaks))
    {
      df$SEGMENT <- 1
    } else
    {
      df <- mHealthR::mhealth.segment(df, breaks, file_type = "sensor")
    }

    n_threshold <-
      break_str_to_sample_size(ts = df[, 1],
                               breaks = breaks,
                               sr = sampling_rate(df))
    result <- plyr::ddply(df, c("SEGMENT"), function(rows)
    {
      rows[, 1] <- as.numeric(rows[, 1])
      rows <- stats::na.omit(rows)
      if (nrow(rows) >= 0.9 * n_threshold)
      {
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
        if (type == "trapz")
        {
          auc_values <-
            (plyr::numcolwise(caTools::trapz, x = rows[, 1]))(rows[2:n_cols])
          max_values <- 16 * n_threshold
        } else if (type == "power")
        {
          auc_values <-
            (plyr::numcolwise(caTools::trapz,
                              x = rows[, 1]))(as.data.frame(rows[2:n_cols] ^ 2))
          max_values <- 16 ^ 2 * n_threshold
        } else if (type == "mean_by_time")
        {
          auc_values <-
            (plyr::numcolwise(sum))(rows[2:n_cols]) /
            (max(rows[, 1]) - min(rows[, 1]))
          max_values <- 16 * n_threshold / 32
        } else if (type == "mean_by_size")
        {
          auc_values <-
            (plyr::numcolwise(sum))(rows[2:n_cols]) / length(rows[, 1])
          max_values <- 16
        } else if (type == "sum")
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
      # flag extra huge values
      auc_values[auc_values >= max_values] <- -1
      auc_values[auc_values < 0] <- -1
      return(data.frame(ts = rows[1, 1], auc_values))
    })
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

#' @name aggregate_for_orientation
#' @title Calculate orientation value for each column over a certain break (e.g. hour, min).
#' @note If certain break is not provided or missing, will use the entire sequence. The column name (except for the first column) of output dataframe would be: [SUMMARY\_METHOD]\_INPUT\_HEADER\_NAME.
#' @export
#' @importFrom plyr ddply numcolwise
#' @importFrom caTools trapz
#' @importFrom mHealthR mhealth.segment
#' @importFrom lubridate tz
#' @importFrom stats na.omit
#' @param df input dataframe that matches mhealth sensor data format.
#' @param breaks could be 'sec', 'min', 'hour', 'day', 'week', 'month', 'quarter' or 'year'; or preceded by an interger and a space.
aggregate_for_orientation <-
  function(df,
           breaks,
           epoch = 2,
           unit = "deg")
  {
    time_zone <- lubridate::tz(df[1, 1])
    n_cols <- ncol(df)
    if (missing(breaks) || is.null(breaks))
    {
      df$SEGMENT <- 1
    } else
    {
      df <- mHealthR::mhealth.segment(df, breaks, file_type = "sensor")
    }

    n_threshold <-
      break_str_to_sample_size(ts = df[, 1],
                               breaks = breaks,
                               sr = sampling_rate(df))
    result <- plyr::ddply(df, c("SEGMENT"), function(rows)
    {
      rows <- stats::na.omit(rows)
      if (nrow(rows) >= 0.9 * n_threshold)
      {
        ori_values <-
          compute_orientation(rows[, 1:n_cols], epoch = epoch, unit = unit)
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
    result$SEGMENT <- NULL
    names(result)[1] <- names(df)[1]
    result[1] <-
      as.POSIXct(result[[1]], origin = "1970-01-01", tz = time_zone)
    return(result)
  }
