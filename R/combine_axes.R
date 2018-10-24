#' @name vector_magnitude
#' @title Compute the vector magnitude value of sensor data.
#' @export
#' @importFrom stringr str_split
#' @param df input dataframe that matches specification.
vector_magnitude <- function(df, axes = NULL)
{
  n_cols <- ncol(df)
  vm_data <- df
  stamp_name <- colnames(df)[1]
  tokens <- stringr::str_split(names(df)[2], "_")[[1]]
  label_name <-
    paste(c("MAGNITUDE", tokens[2:length(tokens)]), collapse = "_")
  if (is.null(axes))
  {
    vm_data[label_name] <- rowSums(df[, 2:n_cols] ^ 2)
  } else if (length(axes) == 1)
  {
    vm_data[label_name] <- df[, axes] ^ 2
  } else
  {
    vm_data[label_name] <- rowSums(df[, axes] ^ 2)
  }
  vm_data[, label_name] <- sqrt(vm_data[, label_name])
  vm_data <- vm_data[, c(stamp_name, label_name)]
  return(vm_data)
}

#' @name sum_up
#' @title Compute the sum up value of sensor data.
#' @export
#' @importFrom stringr str_split
#' @param df input dataframe that matches specification.
sum_up <- function(df, axes = NULL)
{
  n_cols <- ncol(df)
  sum_data <- df
  stamp_name <- colnames(df)[1]
  tokens <- stringr::str_split(names(df)[2], "_")[[1]]
  label_name <-
    paste(c("SUMUP", tokens[2:length(tokens)]), collapse = "_")
  if (is.null(axes))
  {
    sum_data[label_name] <- rowSums(df[, 2:n_cols])
  } else if (length(axes) == 1)
  {
    sum_data[label_name] <- df[, axes]
  } else
  {
    sum_data[label_name] <- rowSums(df[, axes])
  }
  sum_data <- sum_data[, c(stamp_name, label_name)]
  return(sum_data)
}
