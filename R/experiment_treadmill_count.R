#' @title function to calculate the count values for treadmill data
#' @importFrom plyr ddply .
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect
#' @importFrom stats na.omit
#' @export
experiment_treadmill_count <-
  function(raw_data,
           epoch = "5 sec",
           noise_level = 0.03,
           cutoffs = c(0.2, 5),
           k = 0.05,
           spar = 0.6,
           integration = "trapz",
           combination = "axis",
           use_extrapolate = TRUE,
           use_interpolate = TRUE,
           use_filtering = TRUE)
  {
    walkrun_data <- raw_data
    para <-
      list(
        EPOCH = epoch,
        LOW_BW = cutoffs[1],
        HIGH_BW = cutoffs[2],
        INTEGRATION = integration,
        COMBINATION = combination,
        SMOOTHING = spar,
        NEIGHBOR = k
      )
    # compute count over epoches
    walkrun_count <-
      walkrun_data %>%
      plyr::ddply(plyr::.(MPH, PID, LOCATION, ID, SR, GRANGE),
                  function(segment)
                  {
                    gr <- as.numeric(segment$GRANGE[1])
                    count <-
                      segment %>% subset(select = 1:4) %>% mims_unit(
                        breaks = para$EPOCH,
                        range = c(-gr, gr),
                        noise_level = noise_level,
                        k = para$NEIGHBOR,
                        spar = para$SMOOTHING,
                        cutoffs = c(para$LOW_BW, para$HIGH_BW),
                        integration = para$INTEGRATION,
                        combination = para$COMBINATION,
                        use_extrapolation = use_extrapolate,
                        use_interpolation = use_interpolate,
                        use_filtering = use_filtering
                      ) %>% stats::na.omit()
                    result <-
                      count %>% cbind(
                        segment %>% subset(select = -(1:4)) %>% unique(),
                        INDEX = 1:nrow(count),
                        stringsAsFactors = FALSE
                      )
                    return(result)
                  },
                  .progress = "none",
                  .inform = TRUE)

    if (para$COMBINATION == "sum")
    {
      count_col <-
        walkrun_count %>%
        colnames() %>%
        stringr::str_detect(pattern = "SUMUP") %>%
        which()
      names(walkrun_count)[count_col] <- "COUNT"
    } else if (para$COMBINATION == "vm")
    {
      count_col <-
        walkrun_count %>%
        colnames() %>%
        stringr::str_detect(pattern = "MAGNITUDE") %>%
        which()
      names(walkrun_count)[count_col] <- "COUNT"
    } else
    {
      count_cols <-
        walkrun_count %>%
        colnames() %>%
        stringr::str_detect(pattern = "_X|_Y|_Z") %>%
        which()
      names(walkrun_count)[count_cols] <- c("X", "Y", "Z")
    }
    return(walkrun_count)
  }
