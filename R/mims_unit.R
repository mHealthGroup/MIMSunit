#' @name mims_unit
#' @title Compute activity counts using [citation] algorithm
#' @return Will return NA if a segment break is too short to have enough samples to compute AUC
#' @param df input sensor data frame that fits the mhealth specification
#' @param breaks epoch to compute counts on
#' @param range dynamic range of the device (two element vector)
#' @param noise_level noise level should be between 0.01 and 0.1
#' @param k neighborhood duration for extrapolation in seconds, default is 0.05 seconds
#' @param spar smoothing parameter for extrapolation, default is 0.6
#' @param resample set 0 to not use resampling, otherwise set to the desired sampling rate in numerical, e.g 40 for 40Hz. Default is 50.
#' @param filter 'butter', 'ellip', 'bessel'
#' @param cutoffs cut off frequencies to be used in filtering, default is 0.2Hz and 5Hz. If choosing bessel, the low pass cut off would be multipled by 2 when being used.
#' @param integration the integration method to be used: 'trapz', 'absoluteMeanByPoints'.
#' @param allow_truncation use zero truncation or not, default is TRUE
#' @export
mims_unit <-
  function(df,
           breaks = "5 sec",
           range,
           noise_level = 0.03,
           k = 0.05,
           spar = 0.6,
           filter_type = "butter",
           cutoffs = c(0.2, 5),
           integration = "trapz",
           axes = c(2, 3, 4),
           use_extrapolation = TRUE,
           use_interpolation = TRUE,
           use_filtering = TRUE,
           combination = "sum",
           vm_after_extrapolation = FALSE,
           allow_truncation = TRUE,
           output_per_axis = FALSE,
           output_orientation = FALSE,
           breaks_for_orientation = NULL,
           before_df = NULL,
           after_df = NULL)
  {
    # save the start and stop time of original df
    start_time <- lubridate::floor_date(df[1, 1], unit = "seconds")
    stop_time <-
      lubridate::floor_date(df[nrow(df), 1], unit = "seconds")

    # concatenate with before and after df
    if (is.data.frame(before_df))
    {
      df <- rbind(before_df, df)
    }
    if (is.data.frame(after_df))
    {
      df <- rbind(df, after_df)
    }

    # apply extrapolation algorithm
    if (use_extrapolation)
    {
      extrapolated_data <-
        extrapolate.data.frame(df, range, noise_level, k, spar)
    } else if (use_interpolation)
    {
      extrapolated_data <-
        interpolate_signal(df, sr = 100, method = "spline_natural")
    } else
    {
      extrapolated_data <- df
    }

    if (vm_after_extrapolation)
    {
      extrapolated_data <-
        vector_magnitude(extrapolated_data, axes = axes)
    }

    sr <- sampling_rate(extrapolated_data)
    # Resample to a consistent sampling rate
    if (FALSE)
    {
      resampled_data <-
        change_sampling_rate(extrapolated_data, origSr = sr, newSr = resample)
      # update to the new sampling rate
      sr <- resample
    } else
    {
      resampled_data <- extrapolated_data
    }

    # store -1 values separately
    row_abnormal <- rep(FALSE, nrow(resampled_data))
    for (i in 2:ncol(resampled_data))
    {
      row_abnormal <- row_abnormal | resampled_data[[i]] < -150
    }

    abnormal_data <- resampled_data[row_abnormal,]
    normal_data <- resampled_data[!row_abnormal,]

    # Apply filter cascade
    if (use_filtering)
    {
      if (filter_type == "butter")
      {
        filtered_data <-
          iir(
            normal_data,
            Fs = sr,
            Fc = cutoffs,
            order = 4,
            type = "pass",
            filter_type = "butter"
          )
      } else if (filter_type == "bessel")
      {
        filtered_data <- average_removal(normal_data, sr = sr, order = 0.5)
        filtered_data <- filtered_data[[1]]
        filtered_data <-
          bessel(
            filtered_data,
            sr = sr,
            cutoff_freq = cutoffs[2] * 2,
            order = 8
          )
      } else if (filter_type == "ellip")
      {
        filtered_data <-
          iir(
            normal_data,
            Fs = sr,
            Fc = cutoffs,
            order = 4,
            type = "pass",
            filter_type = "ellip"
          )
      }
    } else
    {
      filtered_data <- normal_data
    }

    # sort by timestamp
    colnames(abnormal_data)[2:4] <- colnames(filtered_data)[2:4]
    filtered_data <- rbind(filtered_data, abnormal_data)
    filtered_data <-
      filtered_data[order(filtered_data$HEADER_TIME_STAMP), ]

    # Compute orientations
    if (output_orientation)
    {
      if (is.null(breaks_for_orientation))
      {
        breaks_for_orientation <- breaks
      }
      orientation_data <-
        aggregate_for_orientation(filtered_data,
                                  breaks = breaks_for_orientation)
    } else
    {
      orientation_data <- NULL
    }

    # Compute the AUC
    integrated_data <-
      aggregate_for_mims(filtered_data,
                         breaks = breaks,
                         type = integration,
                         rectify = TRUE)

    if (vm_after_extrapolation)
    {
      return(integrated_data)
    }
    # Compute vector magnitude
    row_abnormal <- rep(FALSE, nrow(integrated_data))
    for (i in 2:ncol(integrated_data))
    {
      row_abnormal <- row_abnormal | integrated_data[[i]] < 0
    }
    if (combination == "vm")
      mims_data <-
      vector_magnitude(integrated_data, axes = axes)
    else if (combination == "sum")
    {
      mims_data <- sum_up(integrated_data, axes = axes)
    } else
    {
      mims_data <- sum_up(integrated_data, axes = axes)
    }

    if (output_per_axis)
    {
      mims_data <-
        cbind(mims_data, integrated_data[, 2:ncol(integrated_data)])
      colnames(mims_data)[2:ncol(mims_data)] <-
        c("MIMS_UNIT", "MIMS_UNIT_X", "MIMS_UNIT_Y", "MIMS_UNIT_Z")
      mims_data[row_abnormal, c(2, 3, 4, 5)] <- -0.01
    } else
    {
      colnames(mims_data)[2] <- "MIMS_UNIT"
      mims_data[row_abnormal, 2] <- -0.01
    }


    if (allow_truncation)
    {
      truncate_indices <-
        mims_data[, 2:ncol(mims_data)] > 0 &
        (mims_data[, 2:ncol(mims_data)] <=
           (1e-04 * break_str_to_sample_size(NULL, breaks, sr) / sr))
      truncate_indices <- data.frame(truncate_indices)
      mims_data[, 2:ncol(mims_data)] <-
        sapply(1:(ncol(mims_data) - 1), function(n)
        {
          mims_data[truncate_indices[, n], n + 1] <- 0
          return(mims_data[, n + 1])
        })
    }

    # only keep data between start and end time
    keep_mask <-
      mims_data[[1]] >= start_time & mims_data[[1]] < stop_time
    mims_data <- mims_data[keep_mask, ]

    if (output_orientation)
    {
      return(list(mims = mims_data, orientation = orientation_data))
    } else
    {
      return(mims_data)
    }
  }
