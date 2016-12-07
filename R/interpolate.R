#' @name interpolate
#' @title Interpolate the missing points and unify sampling interval for the input sensor data
#' @export
#' @import akima plyr
interpolate = function(df, method = "spline_natural", sr = 100, st = NULL, et = NULL, polyDegree = 3){
  time_zone = lubridate::tz(df[1,1])
  nRows = nrow(df);
  if(is.null(st)){
    st = df[[1]][1]
  }
  if(is.null(et)){
    et = df[[1]][nRows]
  }
  nCols = ncol(df);
  x_out = seq(from = st, to = et, by = 1/sr)
  ts = df[,1]
  values = df[2:nCols]
  result = adply(values, .margins = 2, function(colData){
    colData = colData[[1]]
    output = switch(method,
                    linear = approx(x = ts, y = colData, xout = x_out),
                    spline_fmm = spline(x = ts, y = colData, xout = x_out, method = "fmm"),
                    spline_natural = spline(x = ts, y = colData, xout = x_out, method = "natural"),
                    aspline_original = aspline(x = ts, y = colData, xout = x_out, method = "original"),
                    aspline_improved = spline(x = ts, y = colData, xout = x_out, method = "improved"))
    t_out <<- output$x
    return(output$y)
  }, .progress = progress_text(), .id = NULL)
  result = data.frame(t(result), row.names = NULL)
  result = cbind(t_out, result)
  names(result) = names(df)
  names(result[2:ncol(result)]) = paste("INTERPOLATED", names(result[2:ncol(result)]), sep = "_")
  result[,1] = as.POSIXct(result[,1], origin = "1970-01-01", tz = time_zone)
  return(result)
}