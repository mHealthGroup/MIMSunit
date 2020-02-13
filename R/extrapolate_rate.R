#' Get extrapolation rate.
#'
#' \code{extrapolate_rate} computes the extrapolation rate given the test signal
#' (maxed out), the true complete signal (no maxed out) and the extrapolated
#' signal.
#'
#' All three input dataframes will have the same format, with the first column
#' being timestamps in POSXlct format, and the following columns being
#' acceleration values in g.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is used to
#'   compute extrapolation rate during extrapolation parameter optimization. You
#'   may see results in Figure 2 of the manuscript.
#'
#' @param test_df dataframe. See details for the input format.
#' @param true_df dataframe. See details for the input format.
#' @param extrap_df dataframe. See details for the input format.
#' @return number. The extrapolation rate value in double format. If
#'   extrapolation rate is 1, it means the extrapolated signal recovers as the
#'   true signal. If extrapolation rate is between 0 and 1, it means the
#'   extrapolation helps reducing the errors caused by signal maxing out. If
#'   extrapolation rate is smaller than 0, it means the extrapolation increases
#'   the errors caused by signal maxing out (during over extrapolation).
#' @family extrapolation related functions
#' @export
#' @examples
#'   # Prepare data for test, ground truth
#'   test_df = conceptual_diagram_data[
#'                 conceptual_diagram_data['GRANGE'] == 4,
#'                 c("HEADER_TIME_STAMP", "X")]
#'   true_df = conceptual_diagram_data[
#'                 conceptual_diagram_data['GRANGE'] == 8,
#'                 c("HEADER_TIME_STAMP", "X")]
#'
#'   # Do extrapolation
#'   extrap_df = extrapolate(test_df, range=c(-4, 4))
#'
#'   # Compute extrapolation rate
#'   extrapolate_rate(test_df, true_df, extrap_df)
extrapolate_rate <- function(test_df, true_df, extrap_df) {
  true_auc <- caTools::trapz(true_df[[1]], abs(true_df[[2]]))
  test_auc <- caTools::trapz(test_df[[1]], abs(test_df[[2]]))
  extrapolated_auc <-
    caTools::trapz(extrap_df[[1]], abs(extrap_df[[2]]))
  test_err <- (true_auc - test_auc) / true_auc
  extrapolated_err <- (true_auc - extrapolated_auc) / true_auc
  extrapolated_rate <- (test_err - abs(extrapolated_err)) / test_err
  return(extrapolated_rate)
}
