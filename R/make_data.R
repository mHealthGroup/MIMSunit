#' @name crop_grange
#' @title Crop grange for sensor data
#' @export
#' @importFrom plyr colwise
#' @importFrom stats rnorm
crop_grange <- function(df,
                        range = NULL,
                        noise_std = 0.03)
{
  if (!is.null(range))
  {
    upper <- range[2]
    lower <- range[1]
    crop_fun <- plyr::colwise(function(col_data)
    {
      col_data[col_data > upper] <-
        upper + stats::rnorm(sum(col_data > upper), 0, noise_std)
      col_data[col_data < lower] <-
        lower + stats::rnorm(sum(col_data < lower), 0, noise_std)
      return(col_data)
    })
    df[, 2:ncol(df)] <- crop_fun(df[2:ncol(df)])
  }
  return(df)
}

#' @name make_sensor_data
#' @title make sensor data given range and sampling rate and old sensor data
#' @export
make_sensor_data <- function(old_data, new_range, new_sr)
{
  if (sampling_rate(old_data) != new_sr)
  {
    new_data <-
      interpolate_signal(old_data, method = "spline_natural", sr = new_sr)
  } else
  {
    new_data <- old_data
  }
  new_data <- crop_grange(new_data, range = new_range)
  return(new_data)
}
