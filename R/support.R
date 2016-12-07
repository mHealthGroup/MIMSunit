#' @export

numeric.equal = function(x, y){
  return(abs(x - y) < .Machine$double.eps^0.5)
}

#' @export
#' @import stringr
break_str_to_sample_size = function(ts, breaks, sr){
  if(missing(breaks) || is.null(breaks)){
    n = length(ts)
    return(n)
  }else if(str_detect(breaks, "sec")){
    tokens = str_split(breaks, pattern = " ")[[1]]
    n = as.numeric(tokens[1]) * sr
  }else if(str_detect(breaks, "min")){
    tokens = str_split(breaks, pattern = " ")[[1]]
    n = as.numeric(tokens[1]) * sr * 60
  }else if(str_detect(breaks, "hour")){
    tokens = str_split(breaks, pattern = " ")[[1]]
    n = as.numeric(tokens[1]) * sr * 3600
  }else if(str_detect(breaks, "day")){
    tokens = str_split(breaks, pattern = " ")[[1]]
    n = as.numeric(tokens[1]) * sr * 3600 * 24
  }
  return(n)
}