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
mims_unit <- function(df, breaks = "5 sec", range, noise_level = 0.03, k = 0.05, spar = 0.6, filter_type = "butter", cutoffs = c(0.2, 5), integration = "trapz",
    axes = c(2, 3, 4), use_extrapolation = TRUE, use_interpolation = TRUE, use_filtering = TRUE, combination = "sum", vm_after_extrapolation = FALSE,
    allow_truncation = TRUE, output_per_axis = FALSE, output_orientation = FALSE, breaks_for_orientation = NULL, before_df = NULL, after_df = NULL)
    {

    # save the start and stop time of original df
    start_time <- lubridate::floor_date(df[1, 1], unit = "seconds")
    stop_time <- lubridate::floor_date(df[nrow(df), 1], unit = "seconds")

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
        extrapolatedData <- extrapolate.data.frame(df, range, noise_level, k, spar)
    } else if (use_interpolation)
    {
        extrapolatedData <- interpolate_signal(df, sr = 100, method = "spline_natural")
    } else
    {
        extrapolatedData <- df
    }

    if (vm_after_extrapolation)
    {
        extrapolatedData <- vector_magnitude(extrapolatedData, axes = axes)
    }

    sr <- sampling_rate(extrapolatedData)
    # Resample to a consistent sampling rate
    if (FALSE)
    {
        resampledData <- change_sampling_rate(extrapolatedData, origSr = sr, newSr = resample)
        # update to the new sampling rate
        sr <- resample
    } else
    {
        resampledData <- extrapolatedData
    }

    # store -1 values separately
    row_abnormal <- rep(FALSE, nrow(resampledData))
    for (i in 2:ncol(resampledData))
    {
        row_abnormal <- row_abnormal | resampledData[[i]] < -150
    }

    abnormalData <- resampledData[row_abnormal, ]
    normalData <- resampledData[!row_abnormal, ]

    # Apply filter cascade
    if (use_filtering)
    {
        if (filter_type == "butter")
        {
            filteredData <- iir(normalData, Fs = sr, Fc = cutoffs, order = 4, type = "pass", filter_type = "butter")
        } else if (filter_type == "bessel")
        {
            filteredData <- average_removal(normalData, Fs = sr, order = 0.5)
            filteredData <- filteredData[[1]]
            filteredData <- bessel(filteredData, Fs = sr, Fc = cutoffs[2] * 2, order = 8)
        } else if (filter_type == "ellip")
        {
            filteredData <- iir(normalData, Fs = sr, Fc = cutoffs, order = 4, type = "pass", filter_type = "ellip")
        }
    } else
    {
        filteredData <- normalData
    }

    # sort by timestamp
    colnames(abnormalData)[2:4] <- colnames(filteredData)[2:4]
    filteredData <- rbind(filteredData, abnormalData)
    filteredData <- filteredData[order(filteredData$HEADER_TIME_STAMP), ]

    # Compute orientations
    if (output_orientation)
    {
        if (is.null(breaks_for_orientation))
        {
            breaks_for_orientation <- breaks
        }
        orientationData <- aggregate_for_orientation(filteredData, breaks = breaks_for_orientation)
    } else
    {
        orientationData <- NULL
    }

    # Compute the AUC
    integratedData <- aggregate_for_mims(filteredData, breaks = breaks, type = integration, rectify = TRUE)

    if (vm_after_extrapolation)
    {
        return(integratedData)
    }
    # Compute vector magnitude
    row_abnormal <- rep(FALSE, nrow(integratedData))
    for (i in 2:ncol(integratedData))
    {
        row_abnormal <- row_abnormal | integratedData[[i]] < 0
    }
    if (combination == "vm")
        countsData <- vector_magnitude(integratedData, axes = axes) else if (combination == "sum")
        {
        countsData <- sum_up(integratedData, axes = axes)
    } else
    {
        countsData <- sum_up(integratedData, axes = axes)
    }

    if (output_per_axis)
    {
        countsData <- cbind(countsData, integratedData[, 2:ncol(integratedData)])
        colnames(countsData)[2:ncol(countsData)] <- c("MIMS_UNIT", "MIMS_UNIT_X", "MIMS_UNIT_Y", "MIMS_UNIT_Z")
        countsData[row_abnormal, c(2, 3, 4, 5)] <- -0.01
    } else
    {
        colnames(countsData)[2] <- "MIMS_UNIT"
        countsData[row_abnormal, 2] <- -0.01
    }


    if (allow_truncation)
    {
        truncate_indices <- countsData[, 2:ncol(countsData)] > 0 & (countsData[, 2:ncol(countsData)] <= (1e-04 * break_str_to_sample_size(NULL, breaks,
            sr)/sr))
        truncate_indices <- data.frame(truncate_indices)
        countsData[, 2:ncol(countsData)] <- sapply(1:(ncol(countsData) - 1), function(n)
        {
            countsData[truncate_indices[, n], n + 1] <- 0
            return(countsData[, n + 1])
        })
    }

    # only keep data between start and end time
    keep_mask <- countsData[[1]] >= start_time & countsData[[1]] < stop_time
    countsData <- countsData[keep_mask, ]

    # countsData = countsData[!is.na(countsData[,2]),]
    if (output_orientation)
    {
        return(list(mims = countsData, orientation = orientationData))
    } else
    {
        return(countsData)
    }
}
