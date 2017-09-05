#' @title function to calculate the count values for treadmill data
#' @import plyr dplyr
#' @export
experiment_treadmill_count = function(raw_data,
                          epoch = "5 sec",
                          noise_level = 0.03,
                          cutoffs = c(0.2, 5),
                          k = 0.05,
                          spar = 0.6,
                          integration = "trapz",
                          aggregation = 'axis',
                         use_extrapolate = TRUE,
                         use_interpolate = TRUE,
                         use_resampling = FALSE,
                         use_filtering = TRUE){
  walkrun_data = raw_data
  para = list(EPOCH = epoch,
              LOW_BW = cutoffs[1],
              HIGH_BW = cutoffs[2],
              INTEGRATION = integration,
              AGGREGATION = aggregation,
              SMOOTHING = spar,
              NEIGHBOR = k)
  # compute count over epoches
  walkrun_count = walkrun_data %>% ddply(.(MPH, PID, LOCATION, ID, SR, GRANGE), function(segment) {
      gr = as.numeric(segment$GRANGE[1])
      count = segment %>%
        subset(select = 1:4) %>%
        activity_count(
          breaks = para$EPOCH,
          range = c(-gr, gr),
          noise_level = noise_level,
          k = para$NEIGHBOR,
          spar = para$SMOOTHING,
          cutoffs = c(para$LOW_BW, para$HIGH_BW),
          integration = para$INTEGRATION,
          aggregation = para$AGGREGATION,
          use_extrapolation = use_extrapolate,
          use_interpolation = use_interpolate,
          use_resampling = use_resampling,
          use_filtering = use_filtering
        ) %>%
        na.omit
      result = count %>% cbind(
        segment %>% subset(select = -(1:4)) %>% unique,
        INDEX = 1:nrow(count),
        stringsAsFactors = FALSE
      )
      return(result)
    },
    .progress = "none",
    .inform = TRUE)

  countCol = walkrun_count %>% colnames %>% str_detect(pattern = "MAGNITUDE") %>% which
  names(walkrun_count)[countCol] = "COUNT"

  return(walkrun_count)
}