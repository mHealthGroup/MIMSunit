require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(ggthemes)
require(doSNOW)
cl = makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

filename = "reproduce/extdata/treadmill1.rds"
walkrun1 = readRDS(filename)

settings = data.frame(
  resample = c(50, 50, 50),
  low_bandwidth = c(0.2, 0.2, 0.25),
  high_bandwidth = c(5, 5, 2.5),
  use_extrapolate = c(TRUE, FALSE, FALSE),
  use_interpolate = c(TRUE, TRUE, TRUE),
  name = c(
    "Proposed",
    "Proposed without extrapolation",
    "Proposed with narrow passband (0.25-2.5Hz) \n and without extrapolation"
  )
)

k = 0.05
spar = 0.6
noise_level = 0.03

device_settings = data.frame(
  RANGE = c(2, 6, 6, 2, 3, 4, 8, 8),
  SR = c(20, 40, 80, 100, 30, 50, 60, 100),
  ID = c(
    "ActivPal3",
    "GT3XBT",
    "GT3XBT",
    "LG Urbane R",
    "GT3X",
    "Nexus 4",
    "GT9X",
    "GT9X"
  ),
  stringsAsFactors = FALSE
)
selected_gt <- walkrun1 %>% dplyr::filter(SR == "100" & GRANGE == "8")
walkrun1_merged = adply(device_settings, 1, function(device) {

  new_data = ddply(selected_gt, .(MPH, SUBJECT, SESSION, WEIGHTS, LOCATION), function(segment) {
    require(plyr)
    require(dplyr)

    new_segment = segment %>%
      subset(select = 1:4) %>%
      Counts::simulate_new_data(
        new_range = c(-device$RANGE, device$RANGE),
        new_sr = device$SR
      )
    return(new_segment)
  }, .parallel = TRUE)

  new_data$ID = device$ID
  new_data$GRANGE = as.character(device$RANGE)
  new_data$SR = as.character(device$SR)
  return(new_data)
}, .progress = progress_text(), .id = NULL)

walkrun1_merged = walkrun1_merged[c(
  "HEADER_TIME_STAMP",
  "X",
  "Y",
  "Z",
  "ID",
  "GRANGE",
  "SR",
  "LOCATION",
  "WEIGHTS",
  "MPH",
  "SUBJECT",
  "SESSION"
)]

walkrun2_merged <- rbind(walkrun1_merged, walkrun1 %>% dplyr::filter(SR == "40"))

error_data = adply(settings, 1, function(setting) {
  counts = experiment_treadmill_count(
    walkrun2_merged,
    epoch = '5 sec',
    noise_level = noise_level,
    resample = setting$resample,
    cutoffs = c(setting$low_bandwidth, setting$high_bandwidth),
    k = k,
    spar = spar,
    integration = 'trapz',
    use_extrapolate = setting$use_extrapolate,
    use_interpolate = setting$use_interpolate,
    use_resampling = TRUE,
    use_filtering = TRUE
  )

  gt = counts %>% dplyr::filter(ID == "GT9X") %>% ddply(c("MPH"),
                                                                     summarise,
                                                                     MEAN = mean(COUNT),
                                                                     VAR = var(COUNT))

  error = counts %>% ddply(c("MPH"), function(counts) {
    mph = unique(counts$MPH)
    gt_value = gt[gt$MPH == mph, "MEAN"]
    n_total = nrow(counts)
    cv_counts = counts %>% dplyr::filter(ID != "ActivPal3")
    mse_counts = counts %>% dplyr::filter(ID != "GT9X" &
                                            ID != "ActivPal3")
    n_mse = nrow(mse_counts)
    rmse = sqrt(sum((mse_counts$COUNT - gt_value) ^ 2) / n_mse)
    cv_relative = rmse / gt_value

    cv = sd(cv_counts$COUNT) / mean(cv_counts$COUNT)
    return(data.frame(
      RMSE = rmse,
      CV = cv,
      CV_gt = cv_relative
    ))
  })
  return(error)
}, .progress = progress_text(), .id = NULL)

stopCluster(cl)

error_data = error_data[c(6, 7, 9, 10)] # don't use RMSE


filename = "reproduce/extdata/treadmill1_count_actigraph.rds"
treadmill1_count_actigraph = readRDS(filename)

treadmill1_count_actigraph$name = "Actigraph count algorithm"
treadmill1_count_actigraph = treadmill1_count_actigraph %>% dplyr::filter(!(ID == "GT9X" & SR == "40"))

gt_actigraph_count = treadmill1_count_actigraph %>% dplyr::filter(ID == "GT9X") %>% ddply(c("MPH"),
                                                                                                   summarise,
                                                                                                   MEAN = mean(ACTIGRAPH_COUNT),
                                                                                                   VAR = var(ACTIGRAPH_COUNT))

error_actigraph = treadmill1_count_actigraph %>% ddply(c("MPH"), function(counts) {
  mph = unique(counts$MPH)
  gt_value = gt_actigraph_count[gt_actigraph_count$MPH == mph, "MEAN"]
  n_total = nrow(counts)
  mse_counts = counts %>% dplyr::filter(ID != "GT9X" &
                                          ID != "ActivPal3")
  cv_counts = counts %>% dplyr::filter(ID != "ActivPal3")

  n_mse = nrow(mse_counts)
  rmse = sqrt(sum((mse_counts$ACTIGRAPH - gt_value) ^ 2) / n_mse)
  cv_relative = rmse / gt_value
  cv = sd(cv_counts$ACTIGRAPH) / mean(cv_counts$ACTIGRAPH)
  return(data.frame(RMSE = rmse, CV = cv, CV_gt = cv_relative))
})

error_actigraph$name = "Actigraph count algorithm"
error_actigraph = error_actigraph[c(5, 1, 3, 4)]
error_data$name = as.character(error_data$name)
error_data_merged = rbind(error_data, error_actigraph)
error_data_merged = error_data_merged[c(1, 2, 3)]
error_data_melted = error_data_merged %>% dplyr::filter(
  !name == "Proposed with narrower passband (0.25-2.5Hz)\n and without extrapolation, resample to 10Hz"
) %>% melt(c("MPH", "name"))

p = ggplot(error_data_melted,
           aes(
             x = MPH,
             y = value,
             linetype = name,
             shape = name
           )) +

  geom_line() +
  geom_point(size = 3) +

  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.margin = unit(10 ^ -3, 'inch'),
    panel.margin = unit(0.1, 'inch'),
    axis.text = element_text(margin = margin(0, 0, 0, 0)),
    plot.margin = margin(0, 0, 0, 0, 'inch'),
    strip.background = element_blank(),
    legend.key = element_blank()
  ) +
  # facet_wrap(~ variable, scales = "free_y") +
  guides(shape = guide_legend(ncol = 2, title = NULL),
         linetype = guide_legend(ncol = 2, title = NULL)) +
  ylab("Coefficient of variance (CV)") +
  xlab("Frequency (Hz)")

ggsave(
  filename = "shaker_consistency_error.png",
  plot = p,
  path = "reproduce/figure/",
  scale = 2,
  width = 4,
  height = 2
)
