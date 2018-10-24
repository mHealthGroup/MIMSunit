#' @export
numeric.equal <- function(x, y)
{
  return(abs(x - y) < .Machine$double.eps ^ 0.5)
}

#' @export
#' @importFrom stringr str_detect str_split
break_str_to_sample_size <- function(ts, breaks, sr)
{
  if (missing(breaks) || is.null(breaks))
  {
    n <- length(ts)
    return(n)
  } else if (stringr::str_detect(breaks, "sec"))
  {
    tokens <- stringr::str_split(breaks, pattern = " ")[[1]]
    n <- as.numeric(tokens[1]) * sr
  } else if (stringr::str_detect(breaks, "min"))
  {
    tokens <- stringr::str_split(breaks, pattern = " ")[[1]]
    n <- as.numeric(tokens[1]) * sr * 60
  } else if (stringr::str_detect(breaks, "hour"))
  {
    tokens <- stringr::str_split(breaks, pattern = " ")[[1]]
    n <- as.numeric(tokens[1]) * sr * 3600
  } else if (stringr::str_detect(breaks, "day"))
  {
    tokens <- stringr::str_split(breaks, pattern = " ")[[1]]
    n <- as.numeric(tokens[1]) * sr * 3600 * 24
  }
  return(n)
}

#' @name sampling_rate
#' @title Get sensor data's sampling rate from the time difference of adjacent samples
#' @importFrom magrittr %>%
#' @importFrom dplyr last first
#' @export
sampling_rate <- function(df)
{
  duration <- as.numeric(dplyr::last(df[, 1]) - dplyr::first(df[, 1]),
                         units = "secs")
  sr <- round(nrow(df) / duration / 10) * 10
  return(sr)
}
