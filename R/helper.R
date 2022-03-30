#' Parse epoch string to the corresponding number of samples it represents.
#'
#' \code{parse_epoch_string} parses the epoch string (e.g. "1 min"), and outputs
#' the corresponding number of samples it represents.
#'
#' This function parses the given epoch string (e.g. "5 secs") and outputs the
#' corresponding number of samples represented by the epoch string.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is used in
#'   \code{\link{aggregate_for_mims}} function and \code{\link{mims_unit}} function.
#'
#' @param epoch_str string. The input epoch str as accepted by \code{breaks}
#'   argument of \code{\link[base]{cut.POSIXt}}.
#' @param sr number. The sampling rate in Hz used to parse the epoch string.
#' @return number. The number of samples represented by the epoch string.
#' @family utility functions
#' @export
#' @examples
#' # 1 min with 80 Hz = 4800 samples
#' parse_epoch_string('1 min', sr=80)
#'
#' # 30 sec with 30 Hz = 900 samples
#' parse_epoch_string('30 sec', sr=30)
#'
#' # 1 hour with 1 Hz = 3600 samples
#' parse_epoch_string('1 hour', sr=1)
#'
#' # 1 day with 10 Hz = 864000 samples
#' parse_epoch_string('1 day', sr=10)
parse_epoch_string <- function(epoch_str, sr) {
  if (stringr::str_detect(epoch_str, "sec")) {
    tokens <- stringr::str_split(epoch_str, pattern = " ")[[1]]
    n <- as.numeric(tokens[1]) * sr
  } else if (stringr::str_detect(epoch_str, "min")) {
    tokens <- stringr::str_split(epoch_str, pattern = " ")[[1]]
    n <- as.numeric(tokens[1]) * sr * 60
  } else if (stringr::str_detect(epoch_str, "hour")) {
    tokens <- stringr::str_split(epoch_str, pattern = " ")[[1]]
    n <- as.numeric(tokens[1]) * sr * 3600
  } else if (stringr::str_detect(epoch_str, "day")) {
    tokens <- stringr::str_split(epoch_str, pattern = " ")[[1]]
    n <- as.numeric(tokens[1]) * sr * 3600 * 24
  }
  return(n)
}

#' Estimate sampling rate for multi-channel signal
#'
#' \code{sampling_rate} estimates the sampling rate based on the average time
#' intervals between adjacent samples for the input multi-channel signal.
#'
#' This function accepts a dataframe of multi-channel signal, computes the
#' duration of the sequence, and gets the sampling rate by dividing the number
#' of samples by it.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is a utility
#'   function that was used in various part in the algorithm whenever we need to
#'   know the sampling rate.
#'
#' @param df dataframe. Input dataframe of the multi-channel signal. The first
#'   column is the timestamps in POSXlct format and the following columns are
#'   accelerometer values.
#' @return number. The estimated sampling rate in Hz.
#' @family utility functions
#' @export
#' @examples
#' # Get the test data
#' df = sample_raw_accel_data
#'
#' # Default sampling rate is 80Hz
#' sampling_rate(df)
#'
#' # Downsample to 30Hz
#' output = bandlimited_interp(df, 80, 30)
#' sampling_rate(output)
#'
#' # Upsampling to 100Hz
#' output = bandlimited_interp(df, 80, 100)
#' sampling_rate(output)
sampling_rate <- function(df) {
  duration <-
    as.numeric(dplyr::last(df[, 1]) - dplyr::first(df[, 1]), units = "secs")
  sr <- round(nrow(df) / duration / 10) * 10
  return(sr)
}

#' Clip dataframe to the given start and stop time
#'
#' \code{clip_data} clips the input sensor dataframe according to the given
#' start and stop time
#'
#' This function accepts a dataframe of multi-channel signal, clips it
#' according to the start_time and stop_time.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is a utility
#'   function that was used in various part in the algorithm whenever we need to
#'   clip a dataframe.
#'
#' @param df dataframe. Input dataframe of the multi-channel signal. The first
#'   column is the timestamps in POSXlct format and the following columns are
#'   accelerometer values.
#' @param start_time POSXlct format or character. Start time for clipping.
#' If it is a character, it should be recognizable by as.POSXlct function.
#' @param stop_time POSXlct format or character. Stop time for clipping.
#' If it is a character, it should be recognizable by as.POSXlct function.
#' @return dataframe. The same format as the input dataframe.
#' @family utility functions
#' @export
#' @examples
#'   default_ops = options()
#'   options(digits.secs=3)
#'   # Use the provided sample data
#'   df = sample_raw_accel_data
#'
#'   # Check the start time and stop time of the dataset
#'   summary(df)
#'
#'   # Use timestamp string to clip 1 second data
#'   start_time = "2016-01-15 11:01:00"
#'   stop_time = "2016-01-15 11:01:01"
#'   output = clip_data(df, start_time, stop_time)
#'   summary(output)
#'
#'   # Use POSIXct timestamp to clip data
#'   start_time = as.POSIXct("2016-01-15 11:01:00")
#'   stop_time = as.POSIXct("2016-01-15 11:01:01")
#'   output = clip_data(df, start_time, stop_time)
#'   summary(output)
#'
#'   # If start and stop time is not in the range of the input data
#'   # return empty data.frame
#'   start_time = "2016-01-15 12:01:00"
#'   stop_time = "2016-01-15 12:01:01"
#'   output = clip_data(df, start_time, stop_time)
#'   output
#'
#'   # Restore original options
#'   options(default_ops)
clip_data <- function(df, start_time, stop_time) {
  df = as.data.frame(df)
  tzone <- lubridate::tz(df[[1, 1]])
  if (is.character(start_time)) {
    start_time <- as.POSIXct(start_time, tz = tzone)
  }
  start_time <- lubridate::force_tz(start_time, tzone = tzone)
  if (is.character(stop_time)) {
    stop_time <- as.POSIXct(stop_time, tz = tzone)
  }
  stop_time <- lubridate::force_tz(stop_time, tzone = tzone)

  mask <- df[, 1] >= start_time &
    df[, 1] <= stop_time
  sub_df <- df[mask, ]

  return(sub_df)
}

#' Segment input dataframe into windows as specified by breaks.
#' \code{segment_data} segments the input sensor dataframe into
#'   epoch windows with length specified in breaks.
#'
#'   This function accepts a dataframe of multi-channel signal, segments it
#'   into epoch windows with length specified in breaks.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is a utility
#'   function that was used in various part in the algorithm whenever we need to
#'   segment a dataframe, e.g., before aggregating values over epoch windows.
#'
#' @param df dataframe. Input dataframe of the multi-channel signal. The first
#'   column is the timestamps in POSXlct format and the following columns are
#'   accelerometer values.
#' @param breaks character. An epoch length character that can be accepted by
#'   cut.breaks function.
#' @param st character or POSIXct timestamp. An optional start time you can set to
#'   force the breaks generated by referencing this start time. If it is NULL, the
#'   function will use the first timestamp in the timestamp column as start time to
#'   generate breaks. This is useful when you are processing a stream of data and
#'   want to use a common start time for segmenting data. Default is NULL.
#'
#' @return dataframe. The same format as the input dataframe, but with an extra
#'   column "SEGMENT" in the end specifies the epoch window a sample belongs to.
#' @family utility functions
#' @export
#' @examples
#'   # Use sample data
#'   df = sample_raw_accel_data
#'
#'   # segment data into 1 minute segments
#'   output = segment_data(df, "1 min")
#'
#'   # check the 3rd segment, each segment would have 1 minute data
#'   summary(output[output['SEGMENT'] == 3,])
#'
#'   # segment data into 15 second segments
#'   output = segment_data(df, "15 sec")
#'
#'   # check the 1st segment, each segment would have 15 second data
#'   summary(output[output['SEGMENT'] == 1,])
#'
#'   # segment data into 1 hour segments
#'   output = segment_data(df, "1 hour")
#'
#'   # because the input data has only 15 minute data
#'   # there will be only 1 segment in the output
#'   unique(output['SEGMENT'])
#'   summary(output)
#'
#'   # use manually set start time
#'   output = segment_data(df, "15 sec", st='2016-01-15 10:59:50.000')
#'
#'   # check the 1st segment, because the start time is 10 seconds before the
#'   # start time of the actual data, the first segment will only include 5 second
#'   # data.
#'   summary(output[output['SEGMENT'] == 1,])
segment_data <- function(df, breaks, st=NULL) {
  ts <- df[["HEADER_TIME_STAMP"]]
  tzone <- lubridate::tz(ts[1])
  if (!is.null(st)) {
    if (is.character(st)) {
      st <- as.POSIXct(st, tz = tzone)
    }
    ts = append(st, ts)
  }
  if (missing(breaks) || is.null(breaks)) {
    segments <- ts[1]
  } else {
    ts[1] <- .segment.floor_date(ts[1], breaks)
    segments <- cut(ts, breaks = breaks)
  }
  segments <- as.numeric(segments)
  if (!is.null(st)) {
    df["SEGMENT"] <- segments[2:length(segments)]
  } else {
    df["SEGMENT"] <- segments
  }
  return(df)
}

.segment.floor_date <- function(ts, breaks) {
  if (stringr::str_detect(breaks, "sec")) {
    ts <- lubridate::floor_date(ts, unit = c("second"))
  } else if (stringr::str_detect(breaks, "min")) {
    ts <- lubridate::floor_date(ts, unit = c("minute"))
  } else if (stringr::str_detect(breaks, "hour")) {
    ts <- lubridate::floor_date(ts, unit = c("hour"))
  } else if (stringr::str_detect(breaks, "day")) {
    ts <- lubridate::floor_date(ts, unit = c("day"))
  }
  return(ts)
}

.segment.ceil_date <- function(ts, breaks) {
  if (stringr::str_detect(breaks, "sec")) {
    ts <- lubridate::ceiling_date(ts, unit = c("second"))
  } else if (stringr::str_detect(breaks, "min")) {
    ts <- lubridate::ceiling_date(ts, unit = c("minute"))
  } else if (stringr::str_detect(breaks, "hour")) {
    ts <- lubridate::ceiling_date(ts, unit = c("hour"))
  } else if (stringr::str_detect(breaks, "day")) {
    ts <- lubridate::ceiling_date(ts, unit = c("day"))
  }
  return(ts)
}
