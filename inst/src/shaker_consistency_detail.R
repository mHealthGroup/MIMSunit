require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(doSNOW)
require(Counts)
cl = makeCluster(6)
registerDoSNOW(cl)

shaker2 = readRDS("inst/extdata/shaker2.rds")
shaker3 = readRDS("inst/extdata/shaker3.rds")
shaker4 = readRDS("inst/extdata/shaker4.rds")

k = 0.05
spar = 0.6
acc_factor = 60 / 5 #(accumulate to 60 sec counts, based on 5 sec counts)
noise_level = 0.02

settings = data.frame(
  low_bandwidth = c(0.2, 0.2, 0.25, 0.25),
  high_bandwidth = c(5, 5, 2.5, 2.5),
  use_extrapolate = c(TRUE, FALSE, TRUE, FALSE),
  use_interpolate = c(TRUE, TRUE, TRUE, TRUE),
  name = c(
    "Proposed",
    "Proposed without extrapolation",
    "Proposed with narrow passband (0.25-2.5Hz)",
    "Proposed with narrow passband (0.25-2.5Hz) \n and without extrapolation"
  )
)

shaker_raw = rbind(shaker2, shaker3, shaker4)

stat_data = adply(settings, 1, function(setting) {
  require(plyr)
  counts = experiment_shaker_count(
    shaker_raw,
    epoch = '5 sec',
    noise_level = noise_level,
    cutoffs = c(setting$low_bandwidth, setting$high_bandwidth),
    k = k,
    spar = spar,
    integration = 'trapz',
    use_extrapolate = setting$use_extrapolate,
    use_interpolate = setting$use_interpolate,
    use_filtering = TRUE,
    axes = c(2, 3, 4)
  )

  stat = counts %>% ddply(
    c("DEVICE", "GRANGE", "SR", "RPM"),
    summarise,
    N = length(COUNT),
    mean = mean(COUNT * acc_factor),
    sd = sd(COUNT * acc_factor),
    se   = sd / sqrt(N),
    cv = sd / mean
  )
  return(stat)
}, .progress = progress_text(), .id = NULL, .parallel = FALSE)
stat_data$name = as.character(stat_data$name)
stat_data$SERIES = factor(paste0(
  stat_data$DEVICE, ": ",
  stat_data$SR, "Hz, ",
  stat_data$GRANGE, "g"
))
stopCluster(cl)

shaker2_actigraph = readRDS("inst/extdata/shaker2_count_actigraph.rds")
shaker3_actigraph = readRDS("inst/extdata/shaker3_count_actigraph.rds")
shaker4_actigraph = readRDS("inst/extdata/shaker4_count_actigraph.rds")

shaker_actigraph = rbind(shaker2_actigraph, shaker3_actigraph, shaker4_actigraph)


stat_actigraph = shaker_actigraph %>% ddply(
  c("DEVICE", "GRANGE", "SR", "RPM"),
  summarise,
  N = length(ACTIGRAPH),
  mean = mean(ACTIGRAPH * acc_factor),
  sd   = sd(ACTIGRAPH * acc_factor),
  se   = sd / sqrt(N),
  cv = sd / mean
)

stat_actigraph$name = "Actigraph count algorithm"
stat_actigraph$low_bandwidth = 0.25
stat_actigraph$high_bandwidth = 2.5
stat_actigraph$use_extrapolate = TRUE
stat_actigraph$use_interpolate = TRUE

# stat_data = stat_data %>% filter(!(DEVICE == "TAS1E23150139" & SR == 60))
# stat_actigraph = stat_actigraph %>% filter(!(DEVICE == "TAS1E23150139" & SR == 60))
#
# Don't use ActivPal for actigraph count algorithm, meaningless
stat_actigraph = stat_actigraph %>% dplyr::filter(!(DEVICE == "ActivPal3"))
stat_actigraph$SERIES = factor(paste0(
  stat_actigraph$DEVICE, ": ",
  stat_actigraph$SR, "Hz, ",
  stat_actigraph$GRANGE, "g"
))

shaker2_biobank = readRDS("inst/extdata/shaker2_biobank_enmo.rds")
shaker3_biobank = readRDS("inst/extdata/shaker3_biobank_enmo.rds")
shaker4_biobank = readRDS("inst/extdata/shaker4_biobank_enmo.rds")

shaker_biobank = rbind(shaker2_biobank, shaker3_biobank, shaker4_biobank)


stat_biobank = shaker_biobank %>% ddply(
  c("DEVICE", "GRANGE", "SR", "RPM"),
  summarise,
  N = length(biobank_enmo),
  mean = mean(biobank_enmo * acc_factor),
  sd   = sd(biobank_enmo * acc_factor),
  se   = sd / sqrt(N),
  cv = sd / mean
)

stat_biobank$name = "Biobank ENMO algorithm"
stat_biobank$low_bandwidth = 0
stat_biobank$high_bandwidth = 20
stat_biobank$use_extrapolate = FALSE
stat_biobank$use_interpolate = TRUE
stat_biobank$SERIES = factor(paste0(
  stat_biobank$DEVICE, ": ",
  stat_biobank$SR, "Hz, ",
  stat_biobank$GRANGE, "g"
))

stat_data_merged = rbind(stat_data, stat_actigraph, stat_biobank)


stat_data_merged$name = factor(stat_data_merged$name, levels = c(c(
  "Actigraph count algorithm",
  "Proposed with narrow passband (0.25-2.5Hz)",
  "Proposed with narrow passband (0.25-2.5Hz) \n and without extrapolation",
  "Proposed",
  "Proposed without extrapolation"
)))

stat_data_save = stat_data_merged
stat_data_save$name = as.character(stat_data_save$name)

stat_data_save[stat_data_save$name == "Proposed with narrow passband (0.25-2.5Hz) \n and without extrapolation", "name"] = "Proposed with narrow passband (0.25-2.5Hz) and without extrapolation"

current_run = format(Sys.time(), "%Y%m%d%H")

write.csv(stat_data_save, file = paste0("inst/table/shaker_consistency_detail_", current_run,".csv"), quote = FALSE, row.names = FALSE)

p = ggplot(data = stat_data_merged, aes(
  x = RPM,
  y = mean,
  color = SERIES,
  linetype = SERIES
)) +
  geom_line() + geom_point(aes(shape = SERIES)) +
  scale_colour_manual(name = "Device", values = rep("black", 8), guide=FALSE) +
  # scale_linetype_discrete(name = "Device") +
  facet_wrap(~name, ncol = 3, scales = "free_y") +
  guides(shape = guide_legend(ncol = 3, title = NULL),
         linetype = guide_legend(ncol = 3, title = NULL)) +
  ylab("Activity Counts") + xlab("Frequency (Hz)") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.margin = unit(10 ^ -3, 'inch'),
        panel.margin = unit(0.1, 'inch'),
        axis.text = element_text(margin = margin(0, 0, 0, 0)),
        plot.margin = margin(0, 0, 0, 0, 'inch'),
        strip.background = element_blank(),
        legend.key = element_blank())

ggsave(
  filename = paste0("shaker_consistency_detail_",current_run,".png"),
  plot = p,
  path = "inst/figure/",
  scale = 1.8,
  width = 5,
  height = 3,
  dpi = 300
)
