#' @export
numeric.equal <- function(x, y)
{
  return(abs(x - y) < .Machine$double.eps ^ 0.5)
}

#' Parse epoch string to the corresponding number of samples it represents.
#'
#' \code{parse_epoch_string} parses the epoch string (e.g. "1 min"), and outputs
#' the corresponding number of samples it represents.
#'
#' This function parses the given epoch string (e.g. "5 secs") and outputs the
#' corresponding number of samples represented by the epoch string.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is used in
#'   \code{\link{aggregate_for_mims}} function.
#'
#' @param epoch_str. string. The input epoch str as accepted by \code{breaks}
#'   argument of \code{\link[base]{cut.POSIXt}}.
#' @param sr. number. The sampling rate in Hz used to parse the epoch string.
#' @return number. The number of samples represented by the epoch string.
#' @family utility functions
#' @export
parse_epoch_string <- function(epoch_str, sr)
{
  if (stringr::str_detect(epoch_str, "sec"))
  {
    tokens <- stringr::str_split(epoch_str, pattern = " ")[[1]]
    n <- as.numeric(tokens[1]) * sr
  } else if (stringr::str_detect(epoch_str, "min"))
  {
    tokens <- stringr::str_split(epoch_str, pattern = " ")[[1]]
    n <- as.numeric(tokens[1]) * sr * 60
  } else if (stringr::str_detect(epoch_str, "hour"))
  {
    tokens <- stringr::str_split(epoch_str, pattern = " ")[[1]]
    n <- as.numeric(tokens[1]) * sr * 3600
  } else if (stringr::str_detect(epoch_str, "day"))
  {
    tokens <- stringr::str_split(epoch_str, pattern = " ")[[1]]
    n <- as.numeric(tokens[1]) * sr * 3600 * 24
  }
  return(n)
}

#' Estimate sampling rate for multi-channel signal
#'
#' \code{sampling_rate} estimates the sampling rate based on the average time
#' intervals between adjacent samples for the input multi-channel singal.
#'
#' This function accepts a dataframe of multi-channel signal, computes the
#' duration of the sequence, and gets the sampling rate by deviding the number
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
sampling_rate <- function(df)
{
  duration <-
    as.numeric(dplyr::last(df[, 1]) - dplyr::first(df[, 1]), units = "secs")
  sr <- round(nrow(df) / duration / 10) * 10
  return(sr)
}
