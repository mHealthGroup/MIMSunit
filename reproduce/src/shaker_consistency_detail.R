require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(doSNOW)
require(SMARTcounts)

shaker_experiments_clean = readRDS("reproduce/extdata/shakers_mims_algorithms.rds")
acc_factor = 60 / 5;
stat_data = shaker_experiments_clean %>% ddply(c("DEVICE", "GRANGE", "SR", "RPM", 'name'),
    summarise,
    N = length(SMART_COUNTS),
    mean = mean(SMART_COUNTS * acc_factor),
    sd = sd(SMART_COUNTS * acc_factor),
    se   = sd / sqrt(N),
    cv = sd / mean
)

stat_data$name = as.character(stat_data$name)
stat_data$SERIES = factor(paste0(
  stat_data$DEVICE, ": ",
  stat_data$SR, "Hz, ",
  stat_data$GRANGE, "g"
))

shaker2_actigraph = readRDS("reproduce/extdata/shaker2_count_actigraph.rds")
shaker3_actigraph = readRDS("reproduce/extdata/shaker3_count_actigraph.rds")
shaker4_actigraph = readRDS("reproduce/extdata/shaker4_count_actigraph.rds")
shaker5_actigraph = readRDS("reproduce/extdata/shaker5_count_actigraph.rds")

shaker_actigraph = rbind(shaker2_actigraph, shaker3_actigraph, shaker4_actigraph, shaker5_actigraph)


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

stat_actigraph$SERIES = factor(paste0(
  stat_actigraph$DEVICE, ": ",
  stat_actigraph$SR, "Hz, ",
  stat_actigraph$GRANGE, "g"
))

shaker2_biobank = readRDS("reproduce/extdata/shaker2_biobank_enmo.rds")
shaker3_biobank = readRDS("reproduce/extdata/shaker3_biobank_enmo.rds")
shaker4_biobank = readRDS("reproduce/extdata/shaker4_biobank_enmo.rds")
shaker5_biobank = readRDS("reproduce/extdata/shaker5_biobank_enmo.rds")

shaker_biobank = rbind(shaker2_biobank, shaker3_biobank, shaker4_biobank, shaker5_biobank)


stat_biobank = shaker_biobank %>% ddply(
  c("DEVICE", "GRANGE", "SR", "RPM", 'name'),
  summarise,
  N = length(biobank_enmo),
  mean = mean(biobank_enmo * acc_factor),
  sd   = sd(biobank_enmo * acc_factor),
  se   = sd / sqrt(N),
  cv = sd / mean
)

stat_biobank$SERIES = factor(paste0(
  stat_biobank$DEVICE, ": ",
  stat_biobank$SR, "Hz, ",
  stat_biobank$GRANGE, "g"
))

stat_data_merged = rbind(stat_data, stat_actigraph, stat_biobank)


# stat_data_merged$name = factor(stat_data_merged$name, levels = c(c(
#   "Actigraph count algorithm",
#   "ENMO_calibrated",
#   "ENMO_uncalibrated",
#   "Proposed with narrow passband (0.25-2.5Hz)",
#   "Proposed with narrow passband (0.25-2.5Hz) \n and without extrapolation",
#   "Proposed",
#   "Proposed without extrapolation"
# )))

stat_data_save = stat_data_merged
stat_data_save$name = as.character(stat_data_save$name)

current_run = format(Sys.time(), "%Y%m%d%H")

write.csv(stat_data_save, file = paste0("reproduce/table/shaker_consistency_detail_", current_run,".csv"), quote = FALSE, row.names = FALSE)

p = ggplot(data = stat_data_merged, aes(
  x = RPM,
  y = mean,
  color = SERIES,
  linetype = SERIES
)) +
  geom_line() + geom_point(aes(shape = SERIES)) +
  scale_colour_manual(name = "Device", values = rep("black", 12), guide=FALSE) +
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
  path = "reproduce/figure/",
  scale = 1.8,
  width = 5,
  height = 3,
  dpi = 300
)
