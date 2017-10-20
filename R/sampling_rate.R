#' @name sampling_rate
#' @title Get sensor data's sampling rate from the time difference of adjacent samples
#' @importFrom magrittr %>%
#' @importFrom dplyr last first
#' @export
sampling_rate = function(df){
  duration = as.numeric(df[,1] %>% dplyr::last - df[,1] %>% dplyr::first, units = "secs")
  sr = round(nrow(df)/duration/10)*10
  return(sr)
}