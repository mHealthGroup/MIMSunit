#' @name crop_grange
#' @title Crop grange for sensor data
#' @export
#' @import plyr

crop_grange = function(df, range = NULL, noise_std = 0.03){
  if(!is.null(range)){
    upper = range[2]
    lower = range[1]
    cropFun = colwise(function(colData){
      colData[colData > upper] = upper + rnorm(sum(colData > upper), 0, noise_std)
      colData[colData < lower] = lower + rnorm(sum(colData < lower), 0, noise_std)
      return(colData)
    })
    df[,2:ncol(df)] = cropFun(df[,2:ncol(df)])
  }
  return(df)
}

#' @name make_sensor_data
#' @title make sensor data given range and sampling rate and old sensor data
#' @export
make_sensor_data = function(oldData, new_range, new_sr){
  newData = Counts::interpolate(oldData, method = "spline_natural", sr = new_sr)
  newData = Counts::crop_grange(newData, range = new_range)
  return(newData)
}