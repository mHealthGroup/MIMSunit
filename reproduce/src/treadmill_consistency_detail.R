require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(mHealthR)
require(doSNOW)
require(MIMSunit)
cl = makeCluster(4, type = "SOCK")
registerDoSNOW(cl)
filename = "reproduce/extdata/treadmill.rds"
treadmill_data = readRDS(filename)

# save raw plots in pdf
# p = mhealth.plot_timeseries(list(treadmill_data), file_types = c("sensor"), select_cols = list(c(2:4)), group_cols = c("MPH", "PID", 'ID', 'LOCATION'), ncols = 4)

# ggsave("reproduce/extdata/treadmill.pdf", plot = p, width = 8.5, height = 11)

k = 0.05
spar = 0.6
noise_level = 0.03
acc_factor = 60 / 5

settings = data.frame(
  low_bandwidth = c(0.2
                    # 0.2,
                    # 0.25
                    ),
  high_bandwidth = c(5
                     ),
  use_extrapolate = c(TRUE
                      ),
  use_interpolate = c(TRUE
                      ),
  name = c(
    "Proposed"
  )
)

mims_data = adply(settings, 1, function(setting) {
  require(plyr)
  counts = experiment_treadmill_count(
    treadmill_data,
    epoch = '5 sec',
    cutoffs = c(setting$low_bandwidth, setting$high_bandwidth),
    integration = c("trapz"),
    k = k,
    spar = spar,
    combination = "sum",
    use_extrapolate = setting$use_extrapolate,
    use_interpolate = setting$use_interpolate,
    use_filtering = TRUE
  )
  return(counts)
}, .progress = progress_text(), .id = NULL, .parallel = FALSE)

stopCluster(cl)

saveRDS(mims_data, file = "reproduce/extdata/treadmill_mims.rds")

filename = "reproduce/extdata/treadmill_actigraph.rds"
actigraph_data = readRDS(filename)

actigraph_data$name = "Actigraph count algorithm"
actigraph_stat <- ddply(
  actigraph_data,
  c("ID", "MPH", "name", "LOCATION"),
  summarise,
  N = length(ACTIGRAPH_COUNT),
  mean = mean(ACTIGRAPH_COUNT * acc_factor),
  sd = sd(ACTIGRAPH_COUNT * acc_factor),
  se = sd / sqrt(N),
  cv = sd / mean,
  SR = mean(SR),
  GRANGE = mean(GRANGE)
)

filename = "reproduce/extdata/treadmill_enmo.rds"
enmo_data = readRDS(filename)

enmo_data$name = "Biobank ENMO algorithm"
enmo_stat <- ddply(
  enmo_data,
  c("ID", "MPH", "name", "LOCATION"),
  summarise,
  N = length(ENMO),
  mean = mean(ENMO * acc_factor),
  sd = sd(ENMO * acc_factor),
  se = sd / sqrt(N),
  cv = sd / mean,
  SR = mean(SR),
  GRANGE = mean(GRANGE)
)

mims_stat <-
  ddply(
    mims_data,
    c("ID", "MPH", "name", "LOCATION"),
    summarise,
    N = length(MIMS_UNIT),
    mean = mean(MIMS_UNIT * acc_factor),
    sd = sd(MIMS_UNIT * acc_factor),
    se   = sd / sqrt(N),
    cv = sd / mean,
    SR = mean(SR),
    GRANGE = mean(GRANGE)
  )

mims_stat$name = as.character(mims_stat$name)


stat_data_merged = rbind(mims_stat, actigraph_stat, enmo_stat)

stat_data_merged$SERIES = factor(paste0(
  stat_data_merged$ID,
  ": ",
  stat_data_merged$SR,
  "Hz, ",
  stat_data_merged$GRANGE,
  "g"
))
current_run = format(Sys.time(), "%Y%m%d%H")
write.csv(x = stat_data_merged, file = paste0("reproduce/table/treadmill_consistency_count_formatted",current_run,".csv"), row.names = FALSE, quote = FALSE)

p = ggplot(data = stat_data_merged, aes(
  x = MPH,
  y = mean
)) +
  # geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = ID, alpha = LOCATION)) +
  geom_point(aes(shape = ID)) + geom_line(aes(linetype = LOCATION, color = ID)) +
  facet_wrap( ~ name, ncol = 3, scales = "free_y") +
  theme_minimal(base_family = "Times New Roman", base_size = 16) +
  scale_color_grey(start = 0.2, end = 0.3) +
  scale_fill_grey(start = 0.2, end = 0.6) +
  scale_alpha_discrete(range = c(0.4, 0.6)) +
  ylab("counts/min") +
  xlab("km/h")

ggsave(
  filename = paste0("treadmill_consistency_detail_",current_run,".png"),
  plot = p,
  path = "reproduce/figure/",
  scale = 1.2,
  width = 9,
  height = 2.75,
  dpi = 300
)
