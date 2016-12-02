#' @export

numeric.equal = function(x, y){
  return(abs(x - y) < .Machine$double.eps^0.5)
}