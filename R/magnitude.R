#' @name magnitude
#' @title Compute the magnitude value of sensor data.
#' @export
#' @import plyr
#' @param df input dataframe that matches specification.
magnitude = function(df, axes = NULL){
  nCols = ncol(df)
  magnitudeData = df
  stampName = colnames(df)[1]
  tokens = str_split(names(df)[2], "_")[[1]]
  labelName = paste(c("MAGNITUDE",tokens[2:length(tokens)]),collapse="_")
  if(is.null(axes)){
    magnitudeData[labelName] = rowSums(df[,2:nCols]^2)
  }else if(length(axes) == 1){
    magnitudeData[labelName] = df[,axes]^2
  }else{
    magnitudeData[labelName] = rowSums(df[,axes]^2)
  }
  magnitudeData[,labelName] = sqrt(magnitudeData[,labelName])
  magnitudeData = magnitudeData[, c(stampName, labelName)]
  return(magnitudeData)
}