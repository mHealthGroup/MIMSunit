#' Vector magnitude of multi-channel signal.
#'
#' \code{vector_magnitude} computes the vector magnitude value for each sample
#' (row) of a multi-channel signal.
#'
#' This function takes a dataframe of a multi-channel signal as input, and then
#' computes the 2-norm (vector magnitude) for each row and returns a transformed
#' dataframe with two columns.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is not used in
#'   the released version of MIMS-unit algorithm, but was used to compare the
#'   alternative \code{\link{sum_up}} method when combining MIMS-unit values on
#'   each axis into a single value.
#'
#' @param df dataframe. multi-channel signal, with the first column being the
#'   timestamp in POSXct format.
#' @param axes numerical vector. Specify the column indices for each axis. When
#'   this value is NULL, the function assumes the axes are starting from column
#'   2 to the end. Default is \code{NULL}.
#'
#' @return dataframe. The transformed dataframe will have the same number of
#'   rows as input dataframe but only two columns, with the first being
#'   timestamps and second being the vector magnitude values.
#'
#' @seealso \code{\link{sum_up}}
#' @family transformation functions
#' @export
#' @examples
#'   # Use the first 10 rows of the sample data as an example
#'   df = sample_raw_accel_data[1:10,]
#'   df
#'
#'   # By default, the function will assume columns starting from 2 to be axial
#'   # values.
#'   vector_magnitude(df)
#'
#'   # Or, you may specify the column indices yourself
#'   vector_magnitude(df, axes=c(2,3,4))
#'
#'   # Or, if you only want to consider x and y axes
#'   vector_magnitude(df, axes=c(2,3))
#'
#'   # Or, just return the chosen column
#'   vector_magnitude(df, axes=c(2))
#'
vector_magnitude <- function(df, axes = NULL) {
  n_cols <- ncol(df)
  vm_data <- df
  stamp_name <- colnames(df)[1]
  if (is.null(axes)) {
    axes = 2:n_cols
  }
  tokens <- names(df)[axes]
  label_name <-
    paste(c("MAGNITUDE", tokens), collapse = "_")
  if (length(axes) == 1) {
    vm_data[label_name] <- df[, axes]^2
  } else {
    vm_data[label_name] <- rowSums(df[, axes]^2)
  }
  vm_data[, label_name] <- sqrt(vm_data[, label_name])
  vm_data <- vm_data[, c(stamp_name, label_name)]
  return(vm_data)
}

#' Sum of multi-channel signal.
#'
#' \code{sum_up} computes the sum up value for each sample (row) of a
#' multi-channel signal.
#'
#' This function takes a dataframe of a multi-channel signal as input, and then
#' computes the sum of each row and returns a transformed dataframe with two
#' columns.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is used to
#'   combine MIMS-unit values on each axis into a single value after aggregating
#'   over each epoch using \code{\link{aggregate_for_mims}}.
#'
#' @param df dataframe. multi-channel signal, with the first column being the
#'   timestamp in POSXct format.
#' @param axes numerical vector. Specify the column indices for each axis. When
#'   this value is NULL, the function assumes the axes are starting from column
#'   2 to the end. Default is \code{NULL}.
#'
#' @return dataframe. The transformed dataframe will have the same number of
#'   rows as input dataframe but only two columns, with the first being
#'   timestamps and second being the sum up values.
#'
#' @seealso \code{\link{vector_magnitude}}
#' @family transformation functions
#' @export
#' @examples
#'   # Use the first 10 rows of the sample data as an example
#'   df = sample_raw_accel_data[1:10,]
#'   df
#'
#'   # By default, the function will assume columns starting from 2 to be axial
#'   # values.
#'   sum_up(df)
#'
#'   # Or, you may specify the column indices yourself
#'   sum_up(df, axes=c(2,3,4))
#'
#'   # Or, if you only want to consider x and y axes
#'   sum_up(df, axes=c(2,3))
#'
#'   # Or, just return the chosen column
#'   sum_up(df, axes=c(2))
#'
sum_up <- function(df, axes = NULL) {
  n_cols <- ncol(df)
  stamp_name <- colnames(df)[1]
  if (is.null(axes)) {
    axes = 2:n_cols
  }
  tokens <- names(df)[axes]
  label_name <-
    paste(c("SUMUP", tokens), collapse = "_")
  if (length(axes) == 1) {
    df[label_name] <- df[, axes]
  } else {
    df[label_name] <- rowSums(df[, axes])
  }
  df <- df[, c(stamp_name, label_name)]
  return(df)
}
