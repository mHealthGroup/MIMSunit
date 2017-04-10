filename = "inst/extdata/spades_lab_counts.rds"
spades_lab_counts = readRDS(filename)

# optimize scaling
require(dplyr)
require(stringr)
require(ggplot2)
require(reshape2)
forScaling = dplyr::filter(spades_lab_counts, ACTIVITY_NAME %in% c("walking at 1mph arms on desk",
                                                            "walking at 2mph arms on desk",
                                                            "walking at 3mph",
                                                            "walking at 3mph carrying drink",
                                                            "walking at 3mph carrying bag",
                                                            "walking at 3mph phone talking",
                                                            "walking at 3.5mph") & LOCATION == "DOMINANT WAIST")

forScaling_counts = forScaling %>% dplyr::filter(TYPE == "COUNTS")
forScaling_actigraph = forScaling %>% dplyr::filter(TYPE == "ACTIGRAPH")
countsForScaling = data.frame(count = forScaling_counts$value, actigraph = forScaling_actigraph$value)

regressionResult = lm(formula = actigraph ~ count, data = countsForScaling, na.action = na.omit)
scalingFactor = coef(regressionResult)
acc_factor = 60 / 5
# 51 participants' average, compared with Actigraph and METs
act_list = c("lying",
             "sitting",
             "sitting web browsing",
             "sitting writing",
             "standing",
             "standing web browsing",
             "standing writing",
             "walking at 1mph arms on desk",
             "walking at 2mph arms on desk",
             "walking at 3mph",
             "walking at 3mph carrying drink",
             "walking at 3mph carrying bag",
             "walking at 3mph phone talking",
             "walking at 3.5mph",
             "running at 5.5mph 5% grade",
             "walking outdoor",
             "walking upstairs",
             "walking donwstairs",
             "biking at 300 KPM/Min",
             "biking outdoor",
             "sweeping",
             "laundry",
             "shelf unload",
             "shelf reload",
             "frisbee")

act_list = c("lying",
             "sitting",
             "standing",
             "walking at 1mph arms on desk",
             "walking at 2mph arms on desk",
             "walking at 3mph",
             "walking at 3.5mph",
             "running at 5.5mph 5% grade",
             "walking outdoor",
             "walking upstairs",
             "walking donwstairs",
             "biking at 300 KPM/Min",
             "biking outdoor",
             "sweeping",
             "laundry",
             "shelf unload",
             "shelf reload",
             "frisbee")

mets = c(
  1.3,
  1.5,
  1.8,
  2,
  2.8,
  3.5,
  4.3,
  9.8,
  3.5,
  6,
  5,
  3.5,
  6.8,
  3.8,
  2,
  5,
  5,
  3
)

mets = data.frame(activity = act_list, mets_value = mets)
sorted_mets = mets[order(mets[,"mets_value"]),]

p1Data1 = spades_lab_counts

# merge similar activities
p1Data1[str_detect(p1Data1[,1], "sitting"),1] = "sitting"
p1Data1[str_detect(p1Data1[,1], "standing"),1] = "standing"
p1Data1[str_detect(p1Data1[,1], "walking at 3mph"),1] = "walking at 3mph"

# exclude
p1Data = dplyr::filter(p1Data1, !(ACTIVITY_NAME == "frisbee" & SUBJECT == "SPADES_26"))

# scale to 1 min
count_indices = which(p1Data$TYPE == "COUNTS" | p1Data$TYPE == "COUNTS_MAXEDOUT")
# p1Data[count_indices, "value"] = p1Data[count_indices, "value"] * scalingFactor[2] + scalingFactor[1]
p1Data[,"value"] = p1Data[, "value"] * acc_factor

# exclude actigraph maxed out
p1Data = p1Data %>% dplyr::filter(TYPE != "ACTIGRAPH_MAXEDOUT")

p1Data$ACTIVITY_NAME <- factor(p1Data$ACTIVITY_NAME, levels=sorted_mets[,1])
p1Data$SUBJECT_ID <- as.numeric(str_extract(p1Data$SUBJECT, "[0-9]+"))
p1Data = p1Data %>% mutate(LOCATION_TYPE = str_sub(LOCATION, start = -5, end = -1)) %>% mutate(LOCATION_SIDE = str_sub(LOCATION, start = 1, end = -7))

# summarize
# p1Data_summary <- ddply(p1Data, c("ACTIVITY_NAME", "START_TIME", "STOP_TIME", "LABEL_NAME", "TYPE", 'LOCATION', 'LOCATION_TYPE', 'LOCATION_SIDE'), summarise,
#                N    = length(value),
#                mean = mean(value),
#                sd   = sd(value),
#                se   = sd / sqrt(N)
# )

# scatter + box plot
p1Data = p1Data %>% dplyr::filter(LOCATION == "NON DOMINANT WRIST" | LOCATION == "DOMINANT WAIST")
p1Data$LOCATION = str_to_title(p1Data$LOCATION)
p1 = ggplot(data = p1Data, aes(x = ACTIVITY_NAME, y = value)) +

  geom_point(aes(colour = SUBJECT_ID, group = factor(TYPE)), alpha = 0.2, position = position_jitterdodge(dodge.width=0.75, jitter.width=0.3)) +
  scale_colour_gradientn(colors = terrain.colors(50), guide = FALSE) +
  geom_boxplot(aes(fill = TYPE), outlier.colour = NA, alpha = 0.5) +

  # stat_summary(fun.y = median, fun.ymin = function(x){return(quantile(x, probs = 0.25))}, fun.ymax = function(x){return(quantile(x, probs = 0.75))}, geom = "crossbar", width = 0.3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -45, hjust = 0), legend.position = "bottom") +
  xlab("") +
  ylab("Activity counts") +
  facet_grid(LOCATION ~ .)

# box plot
p2Data = p1Data
legend = guide_legend(title = NULL, ncol = 2)
labels = c("Actigraph counts", "Actigraph counts (2g maxedout)", "Proposed counts", "Proposed counts (2g maxedout)")
p2 = ggplot(data = p2Data, aes(x=ACTIVITY_NAME, y = value)) +
  geom_boxplot(aes(fill = TYPE, linetype = TYPE), outlier.colour = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), legend.position = "bottom",
        legend.margin = unit(10 ^ -3, 'inch'),
        panel.margin = unit(0.1, 'inch'),
        axis.text = element_text(margin = margin(0, 0, 0, 0)),
        plot.margin = margin(0, 0, 0, 0, 'inch'),
        strip.background = element_blank(),
        legend.key = element_blank()) +
  ylim(c(0, 3000)) +
  xlab("") +
  ylab("Counts") +
  scale_fill_manual(labels = labels, values = c("gray60", "gray60", "white", "white"), guide = legend) +
  scale_linetype_manual(labels = labels, values = c("solid", "dashed", "solid", "dashed"), guide = legend) +
  facet_wrap(~LOCATION, ncol = 1, scales = "free_y")

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
# ggsave(plot = p1, filename = file.path("inst/figure/", "activity_boxscatter_counts.png"), device = "png", scale = 3, width = 6, height = 3)
ggsave(plot = p2, filename = file.path("inst/figure/", "spades_lab_counts.png"), device = "png", scale = 0.8, width = 10, height = 8)

####-------------- box plot with different scale ----------
p3Data = p1Data
p3Data$ALGORITHM = str_split(p3Data$TYPE, "_", simplify = TRUE)[,1]
legend = guide_legend(title = NULL, ncol = 2)
p3Data$DEVICE = "GT9X"
p3Data[str_detect(p3Data$TYPE, "MAXEDOUT"), "DEVICE"] = "GT9X truncated at ±2g"
p3 = ggplot(data = p3Data, aes(x=ACTIVITY_NAME, y = value)) +
  geom_boxplot(aes(fill = LOCATION, linetype = DEVICE), outlier.colour = NA, colour = "grey40") +
  theme_bw(base_size = 16, base_family = "Times New Roman") +
  theme(axis.text.x = element_text(angle = -90, hjust = 0), legend.position = "bottom", legend.direction = "vertical",
        legend.margin = unit(10 ^ -3, 'inch'),
        panel.margin = unit(0.1, 'inch'),
        axis.text = element_text(margin = margin(0, 0, 0, 0)),
        plot.margin = margin(0, 0, 0, 0, 'inch'),
        strip.background = element_blank()) +
  xlab("") +
  ylab("Counts") +
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
# ggsave(plot = p1, filename = file.path("inst/figure/", "activity_boxscatter_counts.png"), device = "png", scale = 3, width = 6, height = 3)
ggsave(plot = p3, filename = file.path("inst/figure/", "spades_lab_counts_diff_scales.png"), device = "png", scale = 1.5, width = 5, height = 4)
