#' Estimate the accelerometer orientation
#'
#' \code{compute_orientation} returns a dataframe with accelerometer
#' orientations estimated by
#' \href{https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=1241424}{Mizell,
#' 2003} for the input dataframe.
#'
#' This function accepts a dataframe (in mhealth accelerometer data format) and
#' computes the estimated acclerometer orientations (in x, y, and z angles) for
#' every \code{estimation_window} seconds of the entire sequence, and outputs
#' the mean of these angles. The returned dataframe will have the same format as
#' input dataframe, including four columns, and have the same datetime format as
#' input dataframe in the timestamp column. The orientation estimation method
#' used in the function is based on
#' \href{https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=1241424}{Mizell,
#' 2003}.
#'
#' @section How is it used in mims-unit algorithm?: This function is used in
#'   function (\code{\link{aggregate_for_orientation}}).
#'
#' @param df dataframe. Input multi-channel signal. First column should be
#'   timestamps in POSIXt format.
#' @param estimation_window number. window size in seconds to be used to
#'   estimate orientations. Default is 2 (seconds), as suggested by
#'   \href{https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=1241424}{Mizell,
#'   2003}.
#' @param unit string. The unit of orientation angles. Can be "deg" (degree) or
#'   "rad" (radian). Default is "deg".
#' @return dataframe. The returned dataframe will have the same format as input
#'   dataframe.
#'
#' @family transformation functions
#' @export
#' @examples
#'   # Use first 10 second sample data for testing
#'   df = sample_raw_accel_data
#'   df = clip_data(df, start_time = df[1,1], stop_time = df[1, 1] + 600)
#'
#'   # compute orientation angles in degrees
#'   compute_orientation(df)
#'
#'   # compute orientation angles in radian angles
#'   compute_orientation(df, unit='rad')
compute_orientation <- function(df, estimation_window = 2, unit = "deg") {
  sr <- sampling_rate(df)
  segmented_df <-
    MIMSunit::segment_data(df, breaks = paste(estimation_window, "sec"))
  angles_df <-
    plyr::ddply(segmented_df, c("SEGMENT"), function(rows) {
      if (nrow(rows) < sr * estimation_window * 0.8) {
        return(data.frame(
          ts = rows[1, 1],
          x = NaN,
          y = NaN,
          z = NaN
        ))
      }
      axis_means <- colMeans(rows[, 2:4])
      gravity <- sqrt(sum(axis_means^2))
      angles <- acos(axis_means / gravity)
      if (unit == "deg") {
        angles <- angles * 180 / pi
      }
      return(data.frame(
        ts = rows[1, 1],
        x = angles[1],
        y = angles[2],
        z = angles[3]
      ))
    })
  mean_angles <- colMeans(angles_df[, 3:5], na.rm = TRUE)
  mean_angles_df <-
    data.frame(
      ts = df[1, 1],
      x = mean_angles[[1]],
      y = mean_angles[[2]],
      z = mean_angles[[3]]
    )
  colnames(mean_angles_df) <-
    c("HEADER_TIME_STAMP", "X_ANGLE", "Y_ANGLE", "Z_ANGLE")
  return(mean_angles_df)
}
