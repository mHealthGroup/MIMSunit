#' @name compute_orientation
#' @title Calculate orientation of a sensor in (X,Y,Z degrees) for each axis
#' @export
compute_orientation <- function(df, epoch = 2, unit = "deg")
{
  sr <- sampling_rate(df)
  segmented_df <-
    mHealthR::mhealth.segment(df, paste(epoch, "sec"), file_type = "sensor")
  angles_df <-
    plyr::ddply(segmented_df, c("SEGMENT"), function(rows)
    {
      if (nrow(rows) < sr * epoch * 0.8)
      {
        return(data.frame(
          ts = rows[1, 1],
          x = NaN,
          y = NaN,
          z = NaN
        ))
      }
      axis_means <- colMeans(rows[, 2:4])
      gravity <- sqrt(sum(axis_means ^ 2))
      angles <- acos(axis_means / gravity)
      if (unit == "deg")
      {
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
    data.frame(ts = df[1, 1],
               x = mean_angles[[1]],
               y = mean_angles[[2]],
               z = mean_angles[[3]])
  colnames(mean_angles_df) <-
    c("HEADER_TIME_STAMP", "X_ANGLE", "Y_ANGLE", "Z_ANGLE")
  return(mean_angles_df)
}
