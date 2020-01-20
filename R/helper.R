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
clip_data <- function(df, start_time, stop_time) {
  tzone <- lubridate::tz(df[["HEADER_TIME_STAMP"]][1])
  if (is.character(start_time)) {
    start_time <- as.POSIXct(start_time, tz = tzone)
  }
  start_time <- lubridate::force_tz(start_time, tz = tzone)
  if (is.character(stop_time)) {
    stop_time <- as.POSIXct(stop_time, tz = tzone)
  }
  stop_time <- lubridate::force_tz(stop_time, tz = tzone)

  mask <- df[["HEADER_TIME_STAMP"]] >= start_time &
    df[["HEADER_TIME_STAMP"]] <= stop_time
  sub_df <- df[mask, ]

  return(sub_df)
}

#' Segment input dataframe into windows as specified by breaks.
#' \code{segment_data} segments the input sensor dataframe into
#'  epoch windows with length specified in breaks.
#'
#' This function accepts a dataframe of multi-channel signal, segments it
#' into epoch windows with length specified in breaks.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is a utility
#'   function that was used in various part in the algorithm whenever we need to
#'   segment a dataframe, e.g., before aggregating values over epoch windows.
#'
#' @param df dataframe. Input dataframe of the multi-channel signal. The first
#'   column is the timestamps in POSXlct format and the following columns are
#'   accelerometer values.
#' @param breaks character. An epoch length character that can be accepted by
#' cut.breaks function.
#' @return dataframe. The same format as the input dataframe, but with an extra
#' column "SEGMENT" in the end specifies the epoch window a sample belongs to.
#' @family utility functions
#' @export
segment_data <- function(df, breaks) {
  ts <- df[["HEADER_TIME_STAMP"]]
  if (missing(breaks) || is.null(breaks)) {
    segments <- ts[1]
  } else {
    ts[1] <- .segment.floor_date(ts[1], breaks)
    segments <- cut(ts, breaks = breaks)
  }
  segments <- as.numeric(segments)
  df["SEGMENT"] <- segments
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
