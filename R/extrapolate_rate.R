#' @name extrapolate_rate
#' @title compute extrapolation rate
#' @export
#' @import caTools
extrapolate_rate = function(test_df, true_df, extrap_df){
  true_auc = trapz(true_df[[1]], abs(true_df[[2]]))
  test_auc = trapz(test_df[[1]], abs(test_df[[2]]))
  extrapolated_auc = trapz(extrap_df[[1]], abs(extrap_df[[2]]))
  test_err = (true_auc - test_auc) / true_auc
  extrapolated_err = (true_auc - extrapolated_auc) / true_auc
  extrapolated_rate = (test_err - abs(extrapolated_err)) / test_err
  return(extrapolated_rate)
}