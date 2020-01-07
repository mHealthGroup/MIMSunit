#' Compute Monitor Independent Motion Summary unit (MIMS-unit)
#'
#' \code{mims_unit} computes the Monitor Independent Motion Summary unit for the
#' input multi-channel accelerometer signal. The input signal can be from
#' devices of any sampling rate and dynamic range. Please refer to the
#' manuscript for detailed description of the algorithm. Please refer to
#' functions for the intermediate steps: \code{\link{extrapolate}} for
#' extrapolation, \code{\link{iir}} for filtering,
#' \code{\link{aggregate_for_mims}} for aggregation.
#'
#' @note This function is a wrapper function for the low-level
#'   \code{\link{custom_mims_unit}} function. It has set internal parameters as
#'   described in the manuscript. If you want to run customized algorithm for
#'   MIMSunit or if you want to develop better algorithms based on MIMS-unit
#'   algorithm, please use function \code{\link{custom_mims_unit}} where all
#'   parameters are tunable.
#'
#'   \code{before_df} and \code{after_df} are often set when the accelerometer
#'   data are devided into files of smaller chunk.
#'
#' @section How is it used in MIMS-unit algorithm?: This is the main entry of
#'   MIMS-unit algorithm.
#'
#' @param epoch string. Any format that is acceptable by argument \code{breaks}
#'   in method \code{\link[base]{cut.POSIXt}}.For example, "1 sec", "1 min", "5
#'   sec", "10 min". Default is "5 sec".
#' @param dynamic_range numerical vector. The dynamic ranges of the input
#'   signal. Should be a 2-element numerical vector. \code{c(low, high)}, where
#'   \code{low} is the negative max value the device can reach and \code{high}
#'   is the positive max value the device can reach.
#' @param output_mims_per_axis logical. If it is TRUE, the output MIMS-unit
#'   dataframe will have MIMS-unit values for each axis from the third column.
#'   Default is FALSE.
#' @return dataframe. The MIMS-unit dataframe. The first column is the start
#'   time of each epoch in POSIXct format. The second column is the MIMS-unit
#'   value for the input signal. If \code{output_mims_per_axis} is TRUE, the
#'   third column and then are the MIMS-unit values for each axis of the input
#'   signal.
#'
#' @family Top level API functions
#' @name mims_unit

#' @rdname mims_unit
#' @param files character vector. A list of file paths for raw accelerometer
#'   data organized in order to be processed.
#' @param ... additional parameters passed to the import function when reading
#'   in the data from the files.
#' @export
mims_unit_from_files <-
  function(files,
           epoch = "5 sec",
           dynamic_range,
           output_mims_per_axis = FALSE,
           file_type = 'mhealth', ...) {
    num_of_files = length(files)
    if (file_type == 'mhealth') {
      import_fun = import_mhealth_csv
    } else if (file_type == 'actigraph') {
      import_fun = import_actigraph_csv
    } else {
      stop('Only "mhealth" or "actigraph" file types are supported')
    }
    for (i in 1:num_of_files) {
      df = import_fun(files[i], ...)
    }
  }

#' @rdname mims_unit
#' @param df dataframe. Input multi-channel accelerometer signal.
#' @param before_df dataframe. The multi-channel accelerometer signal comes
#'   before the input signal to be prepended to the input signal during
#'   computation. This is used to eliminate the edge effect during extrapolation
#'   and filtering. If it is \code{NULL}, algorithm will run directly on the
#'   input signal. Default is NULL.
#' @param after_df dataframe. The multi-channel accelerometer signal comes after
#'   the input signal to be append to the input signal. This is used to
#'   eliminate the edge effect during extrapolation and filtering. If it is
#'   \code{NULL}, algorithm will run directly on the input signal. Default is
#'   NULL.
#' @export
mims_unit <-
  function(df,
           before_df = NULL,
           after_df = NULL,
           epoch = "5 sec",
           dynamic_range,
           output_mims_per_axis = FALSE) {
    mims_df <- custom_mims_unit(
      df = df,
      epoch = epoch,
      dynamic_range = dynamic_range,
      noise_level = 0.03,
      k = 0.05,
      spar = 0.6,
      filter_type = "butter",
      cutoffs = c(0.2, 5),
      axes = c(2, 3, 4),
      use_extrapolation = TRUE,
      use_filtering = TRUE,
      combination = "sum",
      allow_truncation = TRUE,
      output_mims_per_axis = output_mims_per_axis,
      output_orientation_estimation = FALSE,
      before_df = before_df,
      after_df = after_df
    )
    return(mims_df)
  }

#' Estimates sensor orientation
#'
#' \code{sensor_orientations} estimates the orientation angles for the input
#' multi-channel accelerometer signal. The input signal can be from devices of
#' any sampling rate and dynamic range. Please refer to function
#' \code{\link{compute_orientation}} for the implementation of the estimation
#' algorithm.
#'
#' @note This function interpolates and extrapolates the signal before
#'   estimating the orientation angles.
#'
#'   \code{before_df} and \code{after_df} are often set when the accelerometer
#'   data are devided into files of smaller chunk.
#'
#' @section How is it used in MIMS-unit algorithm?: This is not included in the
#'   official MIMS-unit algorithm nor the manuscript, but we found it is useful
#'   to know the sensor orientations in addition to the summary of movement.
#'
#' @param df dataframe. Input multi-channel accelerometer signal.
#' @param before_df dataframe. The multi-channel accelerometer signal comes
#'   before the input signal to be prepended to the input signal during
#'   computation. This is used to eliminate the edge effect during extrapolation
#'   and filtering. If it is \code{NULL}, algorithm will run directly on the
#'   input signal. Default is NULL.
#' @param after_df dataframe. The multi-channel accelerometer signal comes after
#'   the input signal to be append to the input signal. This is used to
#'   eliminate the edge effect during extrapolation and filtering. If it is
#'   \code{NULL}, algorithm will run directly on the input signal. Default is
#'   NULL.
#' @param epoch string. Any format that is acceptable by argument \code{breaks}
#'   in method \code{\link[base]{cut.POSIXt}}.For example, "1 sec", "1 min", "5
#'   sec", "10 min". Default is "5 sec".
#' @param dynamic_range numerical vector. The dynamic ranges of the input
#'   signal. Should be a 2-element numerical vector. \code{c(low, high)}, where
#'   \code{low} is the negative max value the device can reach and \code{high}
#'   is the positive max value the device can reach.
#' @return dataframe. The orientation dataframe. The first column is the start
#'   time of each epoch in POSIXct format. The second to fourth columns are the
#'   orientation angles.
#'
#' @family Top level API functions
#' @export
sensor_orientations <-
  function(df,
           before_df = NULL,
           after_df = NULL,
           epoch = "5 sec",
           dynamic_range) {
    ori_df <- custom_mims_unit(
      df = df,
      epoch = epoch,
      dynamic_range = dynamic_range,
      output_orientation_estimation = TRUE,
      epoch_for_orientation_estimation = epoch,
      before_df = before_df,
      after_df = after_df
    )[[2]]
    return(ori_df)
  }


#' Compute both MIMS-unit and sensor orientations with custom settings
#'
#' \code{custom_mims_unit} computes the Monitor Independent Motion Summary unit
#' and estimates the sensor orientations for the input multi-channel
#' accelerometer signal with custom settings. The input signal can be from
#' devices of any sampling rate and dynamic range. Please refer to the
#' manuscript for detailed description of the algorithm. Please refer to
#' functions for the intermediate steps: \code{\link{extrapolate}} for
#' extrapolation, \code{\link{iir}} for filtering,
#' \code{\link{aggregate_for_mims}} and \code{\link{aggregate_for_orientation}}
#' for aggregation.
#'
#' @note This function allows you to run customized algorithm for MIMSunit and
#'   sensor orientations.
#'
#'   \code{before_df} and \code{after_df} are often set when the accelerometer
#'   data are devided into files of smaller chunk.
#'
#' @section How is it used in MIMS-unit algorithm?: This is the low-level entry
#'   of MIMS-unit and orientation estimation algorithm. \code{\link{mims_unit}}
#'   calls this function internally.
#'
#' @param df dataframe. Input multi-channel accelerometer signal.
#' @param epoch string. Any format that is acceptable by argument \code{breaks}
#'   in method \code{\link[base]{cut.POSIXt}}.For example, "1 sec", "1 min", "5
#'   sec", "10 min". Default is "5 sec".
#' @param dynamic_range numerical vector. The dynamic ranges of the input
#'   signal. Should be a 2-element numerical vector. \code{c(low, high)}, where
#'   \code{low} is the negative max value the device can reach and \code{high}
#'   is the positive max value the device can reach.
#' @param noise_level number. The tolerable noise level in \eqn{g} unit, should
#'   be between 0 and 1. Default is 0.03, which applies to most devices.
#' @param k number. Duration of neighborhood to be used in local spline
#'   regression for each side, in seconds. Default is 0.05, as optimized by
#'   MIMS-unit algorithm.
#' @param spar number. Between 0 and 1, to control how smooth we want to fit
#'   local spline regression, 0 is linear and 1 matches all local points.
#'   Default is 0.6, as optimized by MIMS-unit algorithm.
#' @param filter_type string. The type of filter to be applied. Could be
#'   'butter' for butterworth bandpass filter, 'ellip' for elliptic bandpass
#'   filter or 'bessel' for bessel lowpass filter + average removal highpass
#'   filter. Default is "butter".
#' @param cutoffs numerical vector. Cut off frequencies to be used in filtering.
#'   If \code{filter_type} is "bessel", the cut off frequency for lowpass filter
#'   would be multipled by 2 when being used. Default is 0.2Hz and 5Hz.
#' @param axes numerical vector. Indices of columns that specifies the axis
#'   values of the input signal. Default is \code{c(2,3,4)}.
#' @param use_extrapolation logical. If it is TRUE, the function will apply
#'   extrapolation algorithm to the input signal, otherwise it will skip
#'   extrapolation but only linearly interpolate the signal to 100Hz. Default is
#'   TRUE.
#' @param use_filtering logical. If it is TRUE, the function will apply bandpass
#'   filtering to the input signal, otherwise it will skip the filtering.
#'   Default is TRUE.
#' @param combination string. Method to combine MIMS-unit values for each axis.
#'   Could be "sum" for \code{\link{sum_up}} or "vm" for
#'   \code{\link{vector_magnitude}}.
#' @param allow_truncation logical. If it is TRUE, the algorithm will truncate
#'   very small MIMS-unit valus to zero. Default is TRUE.
#' @param output_mims_per_axis logical. If it is TRUE, the output MIMS-unit
#'   dataframe will have MIMS-unit values for each axis from the third column.
#'   Default is FALSE.
#' @param output_orientation_estimation logical. If it is TRUE, the function
#'   will also estimate sensor orientations over each epoch. And the output will
#'   be a list, with the first element being the MIMS-unit dataframe, and the
#'   second element being the sensor orientation dataframe. Default is FALSE.
#' @param epoch_for_orientation_estimation string. string. Any format that is
#'   acceptable by argument \code{breaks} in method
#'   \code{\link[base]{cut.POSIXt}}.For example, "1 sec", "1 min", "5 sec", "10
#'   min". Default is "5 sec". It is independent from \code{epoch} for
#'   MIMS-unit.
#' @param before_df dataframe. The multi-channel accelerometer signal comes
#'   before the input signal to be prepended to the input signal during
#'   computation. This is used to eliminate the edge effect during extrapolation
#'   and filtering. If it is \code{NULL}, algorithm will run directly on the
#'   input signal. Default is NULL.
#' @param after_df dataframe. The multi-channel accelerometer signal comes after
#'   the input signal to be append to the input signal. This is used to
#'   eliminate the edge effect during extrapolation and filtering. If it is
#'   \code{NULL}, algorithm will run directly on the input signal. Default is
#'   NULL.
#' @return dataframe or list. If \code{output_orientation_estimation} is TRUE,
#'   the output will be a list, otherwise the output will be the MIMS-unit
#'   dataframe.
#'
#'   The first element will be the MIMS-unit dataframe, in which the first
#'   column is the start time of each epoch in POSIXct format, and the second
#'   column is the MIMS-unit value for the input signal, and the third column
#'   and on are the MIMS-unit values for each axis of the input signal if
#'   \code{output_mims_per_axis} is TRUE.
#'
#'   The second element will be the orientation dataframe, in which the first
#'   column is the start time of each epoch in POSIXct format, and the second to
#'   fourth column is the estimated orientations for the input signal.
#'
#' @family Top level API functions
#' @export
custom_mims_unit <-
  function(df,
           epoch = "5 sec",
           dynamic_range,
           noise_level = 0.03,
           k = 0.05,
           spar = 0.6,
           filter_type = "butter",
           cutoffs = c(0.2, 5),
           axes = c(2, 3, 4),
           use_extrapolation = TRUE,
           use_filtering = TRUE,
           combination = "sum",
           allow_truncation = TRUE,
           output_mims_per_axis = FALSE,
           output_orientation_estimation = FALSE,
           epoch_for_orientation_estimation = NULL,
           before_df = NULL,
           after_df = NULL) {
    # save the start and stop time of original df
    start_time <- lubridate::floor_date(df[1, 1], unit = "seconds")
    stop_time <-
      lubridate::floor_date(df[nrow(df), 1], unit = "seconds")

    # concatenate with before and after df
    if (is.data.frame(before_df)) {
      df <- rbind(before_df, df)
    }
    if (is.data.frame(after_df)) {
      df <- rbind(df, after_df)
    }

    # apply extrapolation algorithm
    if (use_extrapolation) {
      extrapolated_data <-
        extrapolate(df, dynamic_range, noise_level, k, spar)
    } else {
      extrapolated_data <-
        interpolate_signal(df, sr = 100, method = "linear")
    }
    sr <- sampling_rate(extrapolated_data)
    resampled_data <- extrapolated_data


    # store -1 values separately
    row_abnormal <- rep(FALSE, nrow(resampled_data))
    for (i in 2:ncol(resampled_data))
    {
      row_abnormal <- row_abnormal | resampled_data[[i]] < -150
    }

    abnormal_data <- resampled_data[row_abnormal, ]
    normal_data <- resampled_data[!row_abnormal, ]

    # Apply filter cascade
    if (use_filtering) {
      if (filter_type == "butter") {
        filtered_data <-
          iir(
            normal_data,
            sr = sr,
            cutoff_freq = cutoffs,
            order = 4,
            type = "pass",
            filter_type = "butter"
          )
      } else if (filter_type == "bessel") {
        filtered_data <- remove_average(normal_data, sr = sr, order = 0.5)
        filtered_data <- filtered_data[[1]]
        filtered_data <-
          bessel(
            filtered_data,
            sr = sr,
            cutoff_freq = cutoffs[2] * 2,
            order = 8
          )
      } else if (filter_type == "ellip") {
        filtered_data <-
          iir(
            normal_data,
            sr = sr,
            cutoff_freq = cutoffs,
            order = 4,
            type = "pass",
            filter_type = "ellip"
          )
      }
    } else {
      filtered_data <- normal_data
    }

    # sort by timestamp
    colnames(abnormal_data)[2:4] <- colnames(filtered_data)[2:4]
    filtered_data <- rbind(filtered_data, abnormal_data)
    filtered_data <-
      filtered_data[order(filtered_data$HEADER_TIME_STAMP), ]

    # Compute orientations
    if (output_orientation_estimation) {
      if (is.null(epoch_for_orientation_estimation)) {
        epoch_for_orientation_estimation <- epoch
      }
      orientation_data <-
        aggregate_for_orientation(resampled_data,
          epoch = epoch_for_orientation_estimation
        )
    } else {
      orientation_data <- NULL
    }

    # Compute the AUC
    integrated_data <-
      aggregate_for_mims(
        filtered_data,
        epoch = epoch,
        method = "trapz",
        rectify = TRUE
      )

    if (allow_truncation) {
      truncate_indices <-
        integrated_data[, 2:ncol(integrated_data)] > 0 &
          (integrated_data[, 2:ncol(integrated_data)] <=
            (1e-04 * parse_epoch_string(epoch, sr)))
      truncate_indices <- data.frame(truncate_indices)
      integrated_data[, 2:ncol(integrated_data)] <-
        sapply(1:(ncol(integrated_data) - 1), function(n) {
          integrated_data[truncate_indices[, n], n + 1] <- 0
          return(integrated_data[, n + 1])
        })
    }

    # Compute vector magnitude
    row_abnormal <- rep(FALSE, nrow(integrated_data))
    for (i in 2:ncol(integrated_data))
    {
      row_abnormal <- row_abnormal | integrated_data[[i]] < 0
    }
    if (combination == "vm") {
      mims_data <-
        vector_magnitude(integrated_data, axes = axes)
    } else if (combination == "sum") {
      mims_data <- sum_up(integrated_data, axes = axes)
    } else {
      mims_data <- sum_up(integrated_data, axes = axes)
    }

    if (output_mims_per_axis) {
      mims_data <-
        cbind(mims_data, integrated_data[, 2:ncol(integrated_data)])
      colnames(mims_data)[2:ncol(mims_data)] <-
        c("MIMS_UNIT", "MIMS_UNIT_X", "MIMS_UNIT_Y", "MIMS_UNIT_Z")
      mims_data[row_abnormal, c(2, 3, 4, 5)] <- -0.01
    } else {
      colnames(mims_data)[2] <- "MIMS_UNIT"
      mims_data[row_abnormal, 2] <- -0.01
    }


    # only keep data between start and end time
    keep_mask <-
      mims_data[[1]] >= start_time & mims_data[[1]] < stop_time
    mims_data <- mims_data[keep_mask, ]

    if (output_orientation_estimation) {
      return(list(mims = mims_data, orientation = orientation_data))
    } else {
      return(mims_data)
    }
  }
