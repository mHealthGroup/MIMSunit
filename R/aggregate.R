#' @name aggregate
#' @title Calculate summary value (area under curve) for each column over a certain break (e.g. hour, min).
#' @note If certain break is not provided or missing, will use the entire sequence. The column name (except for the first column) of output dataframe would be: [SUMMARY\_METHOD]\_INPUT\_HEADER\_NAME.
#' @export
#' @importFrom plyr ddply numcolwise
#' @importFrom caTools trapz
#' @importFrom mHealthR mhealth.segment
#' @importFrom lubridate tz
#' @importFrom stats na.omit
#' @param df input dataframe that matches mhealth sensor data format.
#' @param breaks could be "sec", "min", "hour", "day", "week", "month", "quarter" or "year"; or preceded by an interger and a space.
#' @param type "trapz", "power", "sum", "meanBySecond", "meanBySize"
#' @param rectify whether rectify the values before computing AUC, default is TRUE
aggregate = function(df, breaks, type = "trapz", rectify = TRUE){
  time_zone = lubridate::tz(df[1,1])
  nCols = ncol(df)
  if(missing(breaks) || is.null(breaks)){
    df$SEGMENT = 1
  }else{
    df = mHealthR::mhealth.segment(df, breaks, file_type = "sensor")
  }

  nThreshold = break_str_to_sample_size(ts = df[,1], breaks = breaks, sr = sampling_rate(df))
  result = plyr::ddply(df,c("SEGMENT"), function(rows){
    rows[,1] = as.numeric(rows[,1])
    rows = stats::na.omit(rows)
    if(nrow(rows) >= 0.9 * nThreshold){
      if(rectify){
        rows[2:nCols] = plyr::numcolwise(function(col_data){
          col_data[col_data > -150] = abs(col_data[col_data > -150])
          if(any(col_data < 0)){
            col_data = rep(-200, length(col_data))
          }
          return(col_data)
          })(rows[2:nCols])
      }
      if(type == "trapz"){
        aucValues = plyr::numcolwise(caTools::trapz, x = rows[,1])(rows[2:nCols])
        maxValue = 16 * nThreshold
      }else if(type == "power"){
        aucValues = plyr::numcolwise(caTools::trapz, x = rows[,1])(as.data.frame(rows[2:nCols]^2))
        maxValue = 16^2 * nThreshold
      }else if(type == "mean_by_time"){
        aucValues = plyr::numcolwise(sum)(rows[2:nCols])/(max(rows[,1]) - min(rows[,1]))
        maxValue = 16 * nThreshold / 32
      }else if(type == "mean_by_size"){
        aucValues = plyr::numcolwise(sum)(rows[2:nCols])/length(rows[,1])
        maxValue = 16
      }else if(type == "sum"){
        aucValues = plyr::numcolwise(sum)(rows[2:nCols])
        maxValue = 16 * nrows(rows)
      }
    }else{
      aucValues = as.data.frame(lapply(rows, function(x) rep.int(-1, 1)))
      aucValues = aucValues[2:nCols]
      maxValue = 0
    }
    # flag extra huge values
    aucValues[aucValues >= maxValue] = -1
    aucValues[aucValues < 0] = -1
    return(data.frame(ts = rows[1,1], aucValues))
  })
  result$SEGMENT = NULL
  names(result)[1] = names(df)[1]
  for(i in 2:nCols){
    names(result)[i] = paste("AGGREGATED", names(result)[i],sep="_")
  }
  result[1] = as.POSIXct(result[[1]], origin = "1970-01-01", tz = time_zone)
  return(result)
}