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
#'
vector_magnitude <- function(df, axes = NULL) {
  n_cols <- ncol(df)
  vm_data <- df
  stamp_name <- colnames(df)[1]
  tokens <- stringr::str_split(names(df)[2], "_")[[1]]
  label_name <-
    paste(c("MAGNITUDE", tokens[2:length(tokens)]), collapse = "_")
  if (is.null(axes)) {
    vm_data[label_name] <- rowSums(df[, 2:n_cols]^2)
  } else if (length(axes) == 1) {
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
#'
sum_up <- function(df, axes = NULL) {
  n_cols <- ncol(df)
  sum_data <- df
  stamp_name <- colnames(df)[1]
  tokens <- stringr::str_split(names(df)[2], "_")[[1]]
  label_name <-
    paste(c("SUMUP", tokens[2:length(tokens)]), collapse = "_")
  if (is.null(axes)) {
    sum_data[label_name] <- rowSums(df[, 2:n_cols])
  } else if (length(axes) == 1) {
    sum_data[label_name] <- df[, axes]
  } else {
    sum_data[label_name] <- rowSums(df[, axes])
  }
  sum_data <- sum_data[, c(stamp_name, label_name)]
  return(sum_data)
}
