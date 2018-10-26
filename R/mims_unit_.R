#' @name mims_unit_
#' @title Compute mims_unit to output intermediate signals at each step
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
#' @param output 'extrapolation', 'filter'
#' @export
mims_unit_ <- function(df, breaks = "5 sec", range, noise_level = 0.03, k = 0.05, spar = 0.6, filter_type = "butter", cutoffs = c(0.2, 5), integration = "trapz",
    axes = c(2, 3, 4), use_extrapolation = TRUE, use_interpolation = TRUE, use_filtering = TRUE, combination = "sum", vm_after_extrapolation = FALSE,
    before_df = NULL, after_df = NULL, output = "filter")
    {

    # save the start and stop time of original df
    start_time <- df[1, 1]
    stop_time <- df[nrow(df), 1]

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

    if (output == "extrapolation")
    {
        return(extrapolatedData)
    }

    sr <- sampling_rate(extrapolatedData)
    # Resample to a consistent sampling rate
    if (FALSE)
    {
        resampledData <- resample(extrapolatedData, origSr = sr, newSr = resample)
        # update to the new sampling rate
        sr <- resample
    } else
    {
        resampledData <- extrapolatedData
    }

    # Apply filter cascade
    if (use_filtering)
    {
        if (filter_type == "butter")
        {
            filteredData <- iir(resampledData, sr = sr, cutoff_freq = cutoffs, order = 4, type = "pass", filter_type = "butter")
        } else if (filter_type == "bessel")
        {
            filteredData <- remove_average(resampledData, sr = sr, order = 0.5)
            filteredData <- filteredData[[1]]
            filteredData <- bessel(filteredData, sr = sr, cutoff_freq = cutoffs[2] * 2, order = 8)
        } else if (filter_type == "ellip")
        {
            filteredData <- iir(resampledData, sr = sr, cutoff_freq = cutoffs, order = 4, type = "pass", filter_type = "ellip")
        }
    } else
    {
        filteredData <- resampledData
    }

    if (output == "filter")
    {
        return(filteredData)
    }
}
