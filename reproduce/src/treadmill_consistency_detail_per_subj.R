require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(mHealthR)
require(doSNOW)
require(Counts)
cl = makeCluster(6, type = "SOCK")
registerDoSNOW(cl)
filename = "reproduce/extdata/treadmill.rds"
treadmill_data = readRDS(filename)

k = 0.05
spar = 0.6
noise_level = 0.03
scale_factor = 466.5
acc_factor = 60 / 5

settings = data.frame(
  resample = c(50
               # 50,
               # 50
               ),
  low_bandwidth = c(0.2
                    # 0.2,
                    # 0.25
                    ),
  high_bandwidth = c(5
                     # 5,
                     # 2.5
                     ),
  use_extrapolate = c(TRUE
                      # FALSE,
                      # FALSE
                      ),
  use_interpolate = c(TRUE
                      # TRUE,
                      # TRUE
                      ),
  name = c(
    "Proposed"
    # "Proposed without extrapolation",
    # "Proposed with narrow passband (0.25-2.5Hz) \n and without extrapolation"
  )
)

device_settings = data.frame(
  RANGE = c(6, 8),
  SR = c(80, 80),
  ID = c(
    "GT3X+",
    "GT9X"
  ),
  stringsAsFactors = FALSE
)

count_data = adply(settings, 1, function(setting) {
  require(plyr)
  counts = experiment_treadmill_count(
    treadmill_data,
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

filename = "reproduce/extdata/treadmill_actigraph.rds"
actigraph_data = readRDS(filename)

actigraph_data$name = "Actigraph count algorithm"
actigraph_stat <- ddply(
  actigraph_data,
  c("MPH", "name", "LOCATION", "PID"),
  summarise,
  N = length(ACTIGRAPH_COUNT),
  mean = mean(ACTIGRAPH_COUNT * acc_factor),
  sd = sd(ACTIGRAPH_COUNT * acc_factor),
  se = sd / sqrt(N),
  cv = sd / mean
)

count_stat <-
  ddply(
    count_data,
    c("MPH", "name", "LOCATION", "PID"),
    summarise,
    N = length(COUNT),
    mean = mean(COUNT * acc_factor),
    sd = sd(COUNT * acc_factor),
    se   = sd / sqrt(N),
    cv = sd / mean
  )

count_stat$name = as.character(count_stat$name)
stat_data_merged = rbind(count_stat, actigraph_stat)

stat_data_merged$SERIES = factor(paste0(
  stat_data_merged$ID,
  ": ",
  stat_data_merged$SR,
  "Hz, ",
  stat_data_merged$GRANGE,
  "g"
))

# write.csv(x = stat_data_merged, file = "reproduce/table/treadmill_consistency_count_formatted.csv", row.names = FALSE, quote = FALSE)

p = ggplot(data = stat_data_merged, aes(
  x = MPH,
  y = mean
)) +
  # geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = ID, alpha = LOCATION)) +
  geom_point(aes(shape = LOCATION)) + geom_line(aes(linetype = LOCATION, color = factor(PID))) +
  facet_wrap( ~ name, ncol = 2, scales = "free_y") +
  theme_minimal(base_family = "Times New Roman", base_size = 16) +
  # scale_color_grey(start = 0.2, end = 0.3) +
  # scale_fill_grey(start = 0.2, end = 0.6) +
  # scale_alpha_discrete(range = c(0.4, 0.6)) +
  ylab("counts/min") +
  xlab("km/h")

ggsave(
  filename = "treadmill_consistency_detail_per_subj.png",
  plot = p,
  path = "reproduce/figure/",
  scale = 1.2,
  width = 6,
  height = 2.75,
  dpi = 300
)
