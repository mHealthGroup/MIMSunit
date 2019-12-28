
require(dplyr)
require(stringr)
require(ggplot2)
require(reshape2)
require(extrafont)
mims_unit_filename = "reproduce/extdata/spades_lab_mims_unit.rds"
spades_lab_mims_unit = readRDS(mims_unit_filename)
actigraph_counts_filename = "reproduce/extdata/spades_lab_actigraph_counts.rds"
spades_lab_actigraph_counts = readRDS(actigraph_counts_filename)
spades_lab_actigraph_counts$PID = as.numeric(spades_lab_actigraph_counts$PID)

# merge these two dataframes
spades_lab_counts = plyr::join(spades_lab_mims_unit, spades_lab_actigraph_counts)
spades_lab_counts = na.omit(spades_lab_counts)
# optimize scaling

loadfonts(device = 'win')
# forScaling = dplyr::filter(spades_lab_counts, ACTIVITY_NAME %in% c("walking at 1mph arms on desk",
#                                                             "walking at 2mph arms on desk",
#                                                             "walking at 3mph",
#                                                             "walking at 3mph carrying drink",
#                                                             "walking at 3mph carrying bag",
#                                                             "walking at 3mph phone talking",
#                                                             "walking at 3.5mph") & LOCATION == "DOMINANT WAIST")
#
# forScaling_counts = forScaling %>% dplyr::filter(TYPE == "COUNTS")
# forScaling_actigraph = forScaling %>% dplyr::filter(TYPE == "ACTIGRAPH")
# countsForScaling = data.frame(count = forScaling_counts$value, actigraph = forScaling_actigraph$value)
#
# regressionResult = lm(formula = actigraph ~ count, data = countsForScaling, na.action = na.omit)
# scalingFactor = coef(regressionResult)
acc_factor = 60 / 5
act_list = c("Lying",
             "Sitting",
             "Standing",
             "Walk 1 mph",
             "Walk 2 mph",
             "Laundry",
             "Frisbee",
             "Walk 3 mph",
             "Self-paced walk",
             "Bike- 300 kmph/min",
             "Sweeping",
             "Walk 3.5 mph",
             "Downstairs",
             "Reload/unload shelf",
             "Upstairs",
             "Bike outdoor",
             "Run- 5.5 mph")

mets = c(
  1,
  1.3,
  1.3,
  2,
  2.8,
  2,
  3,
  3.5,
  4,
  3.5,
  3.3,
  4.3,
  3.5,
  3.5,
  5,
  7.5,
  9
)

mets = data.frame(activity = act_list, mets_value = mets)
sorted_mets = mets[order(mets[,"mets_value"]),]

p1Data1 = spades_lab_counts

# exclude, WHY?
p1Data = dplyr::filter(spades_lab_counts, !(LABEL_NAME == "Frisbee" & PID == 26))
p1Data = melt(p1Data, id.vars=c('HEADER_TIME_STAMP', 'LABEL_NAME', 'PID', 'LOCATION'), variable.name = "TYPE")
# scale to 1 min
p1Data[,"value"] = p1Data[, "value"] * acc_factor
# p1Data[count_indices, "value"] = p1Data[count_indices, "value"] * scalingFactor[2] + scalingFactor[1]

p1Data$LABEL_NAME <- factor(p1Data$LABEL_NAME, levels=act_list)

####-------------- box plot with different scale ----------
p3Data = p1Data
legend = guide_legend(title = NULL, ncol = 2)
p3Data$ALGORITHM = as.character(p3Data$TYPE)
p3Data$ALGORITHM[str_detect(p3Data$TYPE, "MIMS")] = "MIMS-unit"
p3Data$ALGORITHM[str_detect(p3Data$TYPE, "MIMS")] = "MIMS-unit"
p3Data$DEVICE = "±8g Device"
p3Data[str_detect(p3Data$TYPE, "2g"), "DEVICE"] = "±2g Device"

p3Data$DEVICE = factor(p3Data$DEVICE, c("±8g Device", "±2g Device"))

p3Stats = p3Data %>% group_by(LABEL_NAME, LOCATION, TYPE, DEVICE, ALGORITHM) %>% summarise(
  ylower = quantile(value, 1/4),
  yupper = quantile(value, 3/4),
  ymin = max(min(value), ylower - 1.5 * (yupper - ylower)),
  ymax = min(max(value), yupper + 1.5 * (yupper - ylower)),
  mean = median(value)
)

p33 = ggplot(data = p3Stats, aes(x=LABEL_NAME, ymin = ymin, ymax = ymax, lower=ylower, upper=yupper, middle=mean)) +
  geom_boxplot(aes(fill = LOCATION, linetype = DEVICE), colour = "grey40", stat="identity") +
  scale_fill_grey() +
  theme_bw(base_size = 16, base_family = "Times New Roman") +
  theme(legend.position = "bottom", legend.direction = "vertical",
        legend.margin = unit(10 ^ -3, 'inch'),
        panel.margin = unit(0.2, 'inch'),
        plot.margin = margin(0, 0.5, 0, 0, "inch"),
        axis.text = element_text(margin = margin(0, 0, 0, 0)),
        strip.background = element_blank()) +
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0), strip.text.y = element_blank()) +
  # scale_fill_manual(labels = labels, values = c("gray60", "gray60", "white", "white"), guide = legend) +
  facet_grid(ALGORITHM ~ ., scales = "free_y")


p3 = ggplot(data = p3Data, aes(x=LABEL_NAME, y = value)) +
  geom_boxplot(aes(fill = LOCATION, linetype = DEVICE), outlier.colour = NA, colour = "grey40") +
  scale_fill_grey() +
  theme_bw(base_size = 11, base_family = "Times New Roman") +
  theme(legend.position = "bottom", legend.direction = "vertical",
        legend.margin = unit(10 ^ -3, 'inch'),
        panel.margin = unit(0.2, 'inch'),
        plot.margin = margin(0, 0.5, 0, 0, "inch"),
        axis.text = element_text(margin = margin(0, 0, 0, 0)),
        strip.background = element_blank()) +
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0), strip.text.y = element_blank()) +
  # scale_fill_manual(labels = labels, values = c("gray60", "gray60", "white", "white"), guide = legend) +
  facet_grid(ALGORITHM ~ ., scales = "free_y")

# compensation
# runningData = p1Data %>%
#   dplyr::filter(ACTIVITY_NAME == "running at 5.5mph 5% grade") %>%
#   group_by(LOCATION, TYPE) %>%
#   dplyr::summarise(mean_val = mean(value)) %>%
#   recast(LOCATION ~ TYPE) %>%
#   mutate(ACTIGRAPH_CHANGE = (ACTIGRAPH - ACTIGRAPH_MAXEDOUT)/ACTIGRAPH, COUNTS_CHANGE = (COUNTS - COUNTS_MAXEDOUT)/COUNTS)
#
# frisbeeData = p1Data %>%
#   dplyr::filter(ACTIVITY_NAME == "frisbee") %>%
#   group_by(LOCATION, TYPE) %>%
#   dplyr::summarise(mean_val = mean(value)) %>%
#   recast(LOCATION ~ TYPE) %>%
#   mutate(ACTIGRAPH_CHANGE = (ACTIGRAPH - ACTIGRAPH_MAXEDOUT)/ACTIGRAPH, COUNTS_CHANGE = (COUNTS - COUNTS_MAXEDOUT)/COUNTS)
#
# ggsave(plot = p1, filename = file.path("reproduce/figure/", "activity_boxscatter_counts.png"), device = "png", scale = 3, width = 6, height = 3)
ggsave(plot = p33, filename = file.path("reproduce/figure/", "spades_lab_counts_diff_scales.png"), device = "png", scale = 3, width = 4, height = 2, dpi = 1000)
