#' @name experiment_shaker_count
#' @title function to calculate the count values for shaker data
#' @importFrom plyr ddply .
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect
#' @importFrom stats na.omit
#' @export
experiment_shaker_count <-
  function(raw_data,
           epoch = "5 sec",
           noise_level = 0.03,
           cutoffs = c(0.2, 5),
           k = 0.05,
           spar = 0.6,
           integration = c("trapz"),
           use_extrapolate = TRUE,
           use_interpolate = TRUE,
           use_filtering = TRUE,
           axes = c(2, 3, 4))
  {
    shaker_data <- raw_data

    # prepare expanded data frame of parameter combinations

    para <-
      list(
        EPOCH = epoch,
        LOW_BW = cutoffs[1],
        HIGH_BW = cutoffs[2],
        INTEGRATION = integration,
        SMOOTHING = spar,
        NEIGHBOR = k
      )

    # compute count over epoches
    shaker_count <-
      shaker_data %>%
      plyr::ddply(plyr::.(RPM, DEVICE, GRANGE, LOCATION, SR),
                  function(segment)
                  {
                    gr <- as.numeric(segment$GRANGE[1])
                    count <-
                      segment %>% subset(select = 1:4) %>% activity_count(
                        breaks = para$EPOCH,
                        range = c(-gr, gr),
                        noise_level = noise_level,
                        k = para$NEIGHBOR,
                        spar = para$SMOOTHING,
                        cutoffs = c(para$LOW_BW, para$HIGH_BW),
                        integration = para$INTEGRATION,
                        axes = axes,
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
                    count <- count[-1,]
                    return(result)
                  },
                  .progress = "text",
                  .inform = TRUE,
                  .parallel = FALSE)

    count_col <-
      shaker_count %>%
      colnames() %>%
      stringr::str_detect(pattern = "SUMUP") %>%
      which()
    names(shaker_count)[count_col] <- "COUNT"

    return(shaker_count)
  }
