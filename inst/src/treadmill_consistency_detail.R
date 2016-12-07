require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(mHealthR)
require(doSNOW)
cl = makeCluster(6, type = "SOCK")
registerDoSNOW(cl)
filename = "inst/extdata/treadmill1.rds"
walkrun1 = readRDS(filename)

k = 0.05
spar = 0.6
noise_level = 0.03
scale_factor = 466.5

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
      Counts::make_sensor_data(
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

count_data = adply(settings, 1, function(setting) {
  require(mhealthformatsupportr)
  require(plyr)
  counts = experiment_treadmill_count(
    walkrun2_merged,
    epoch = '5 sec',
    resample = setting$resample,
    cutoffs = c(setting$low_bandwidth, setting$high_bandwidth),
    integration = c("trapz"),
    k = k,
    spar = spar,
    use_extrapolate = setting$use_extrapolate,
    use_interpolate = setting$use_interpolate,
    use_resampling = TRUE,
    use_filtering = TRUE
  )
  return(counts)
}, .progress = progress_text(), .id = NULL, .parallel = FALSE)

stopCluster(cl)

filename = "inst/extdata/treadmill1_count_actigraph.rds"
treadmill1_count_actigraph = readRDS(filename)

treadmill1_count_actigraph$name = "Actigraph count algorithm"
treadmill1_count_actigraph = treadmill1_count_actigraph %>% dplyr::filter(!(ID == "GT9X" & SR == "40"))
stat_actigraph <- ddply(
  treadmill1_count_actigraph,
  c("ID", "GRANGE", "SR", "MPH", "name", "LOCATION"),
  summarise,
  N = length(ACTIGRAPH_COUNT),
  mean = mean(ACTIGRAPH_COUNT),
  sd = sd(ACTIGRAPH_COUNT),
  se = sd / sqrt(N)
)


count_data = count_data %>% dplyr::filter(!(ID == "GT9X" & SR == "40"))
stat_data <-
  ddply(
    count_data,
    c("ID", "GRANGE", "SR", "MPH", "name", "LOCATION"),
    summarise,
    N = length(COUNT),
    mean = mean(COUNT) * scale_factor,
    sd   = sd(COUNT) * scale_factor,
    se   = sd / sqrt(N)
  )

stat_data$name = as.character(stat_data$name)
stat_data_merged = rbind(stat_data, stat_actigraph)

stat_data_merged$SERIES = factor(paste0(
  stat_data_merged$ID,
  ": ",
  stat_data_merged$SR,
  "Hz, ",
  stat_data_merged$GRANGE,
  "g"
))


waist_stat = stat_data_merged %>% dplyr::filter(LOCATION == "DominantWaist")


p = ggplot(data = waist_stat, aes(
  x = MPH * 1.609344,
  y = mean,
  linetype = SERIES
  # color = SERIES
)) +
  theme(
    plot.background = element_rect("white"),
    panel.background = element_rect('white'),
    panel.border = element_rect('white')
  ) +
  stat_smooth(fill = NA, size = 0.3, color = "black", alpha = 0.5) +
  facet_wrap( ~ name, ncol = 2)
  # stat_smooth(data = waist_stat %>% filter(GRANGE == "8" & SR == "100"), aes(x = MPH * 1.609344,

d = ggplot_build(p)
d = d$data[[1]]
d = d[c("x", "y", "group", "PANEL")]

marker = ddply(d, c("group", "PANEL"), function(seg){

  marker = spline(x = seg$x, y = seg$y, xout = unique(waist_stat$MPH)* 1.609344)
  return(data.frame(marker))
})

marker$group = factor(marker$group, labels = c("ActivPal3: 20Hz, 2g", "GT3X: 30Hz, 3g", "GT3XBT: 40Hz, 6g", "GT3XBT: 80Hz, 6g",      "GT9X: 100Hz, 8g","GT9X: 60Hz, 8g","LG Urbane R: 100Hz, 2g", "Nexus 4: 50Hz, 4g"))
marker$PANEL = marker$PANEL %>% as.numeric %>% factor(labels = c("Actigraph count algorithm", "Proposed", "Proposed with narrow passband (0.25-2.5Hz) \n and without extrapolation", "Proposed without extrapolation"))

marker$name = marker$PANEL

p2 = ggplot(marker, aes(x = x, y = y)) +
  geom_point(aes(shape = group)) + geom_line(aes(linetype = group)) +
  scale_linetype_discrete(name = "Device", guide = FALSE) +
  scale_shape_discrete(name = "Device", guide = FALSE) +
  guides(shape = guide_legend(ncol = 3, title = NULL),
         linetype = guide_legend(ncol = 3, title = NULL)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.margin = unit(10 ^ -3, 'inch'),
        panel.margin = unit(0.1, 'inch'),
        axis.text = element_text(margin = margin(0, 0, 0, 0)),
        plot.margin = margin(0, 0, 0, 0, 'inch'),
        strip.background = element_blank(),
        legend.key = element_blank()) +
  xlab("Treadmill speed (km/h)") +
  ylab("Activity counts") +
facet_wrap( ~ name, ncol = 2)
# facet_grid(LOCATION ~ TYPE, labeller = label_value)

ggsave(
  filename = "treadmill_consistency_treadmill.png",
  plot = p2,
  path = "inst/figure/",
  scale = 1.5,
  width = 4,
  height = 3,
  dpi = 300
)
