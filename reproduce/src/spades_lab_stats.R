require(plyr)
require(dplyr)
require(stringr)
filename = "reproduce/extdata/spades_lab_stats.rds"
spades_lab_stats = readRDS(filename)

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

# merge similar activities
spades_lab_stats[str_detect(spades_lab_stats$ACTIVITY_NAME, "sitting"),"ACTIVITY_NAME"] = "sitting"
spades_lab_stats[str_detect(spades_lab_stats$ACTIVITY_NAME, "standing"),"ACTIVITY_NAME"] = "standing"
spades_lab_stats[str_detect(spades_lab_stats$ACTIVITY_NAME, "walking at 3mph"),"ACTIVITY_NAME"] = "walking at 3mph"

spades_lab_stats = spades_lab_stats %>%
  ddply(c("ACTIVITY_NAME", "LOCATION"), summarise,
        SD_AMP = sd(, na.rm = TRUE),
        MIN_AMP = min(AMP, na.rm = TRUE),
        MEAN_AMP = mean(AMP, na.rm = TRUE),
        MEDIAN_AMP = median(AMP, na.rm = TRUE),
        MAX_AMP = max(AMP, na.rm = TRUE),
        SD_DOMINANT_FREQ = sd(DOMINANT_FREQ, na.rm = TRUE),
        MIN_DOMINANT_FREQ = min(DOMINANT_FREQ, na.rm = TRUE),
        MEAN_DOMINANT_FREQ = mean(DOMINANT_FREQ, na.rm = TRUE),
        MEDIAN_DOMINANT_FREQ = median(DOMINANT_FREQ, na.rm = TRUE),
        MAX_DOMINANT_FREQ = max(DOMINANT_FREQ, na.rm = TRUE),
        SD_DOMINANT_FREQ_SECOND = sd(DOMINANT_FREQ_SECOND, na.rm = TRUE),
        MIN_DOMINANT_FREQ_SECOND = min(DOMINANT_FREQ_SECOND, na.rm = TRUE),
        MEAN_DOMINANT_FREQ_SECOND = mean(DOMINANT_FREQ_SECOND, na.rm = TRUE),
        MEDIAN_DOMINANT_FREQ_SECOND = median(DOMINANT_FREQ_SECOND, na.rm = TRUE),
        MAX_DOMINANT_FREQ_SECOND = max(DOMINANT_FREQ_SECOND, na.rm = TRUE),
        SD_DOMINANT_FREQ_THIRD = sd(DOMINANT_FREQ_THIRD, na.rm = TRUE),
        MIN_DOMINANT_FREQ_THIRD = min(DOMINANT_FREQ_THIRD, na.rm = TRUE),
        MEAN_DOMINANT_FREQ_THIRD = mean(DOMINANT_FREQ_THIRD, na.rm = TRUE),
        MEDIAN_DOMINANT_FREQ_THIRD = median(DOMINANT_FREQ_THIRD, na.rm = TRUE),
        MAX_DOMINANT_FREQ_THIRD = max(DOMINANT_FREQ_THIRD, na.rm = TRUE)
        )

stats_in_paper = spades_lab_stats %>% filter(LOCATION == "DOMINANT HIP" | LOCATION == "NON DOMINANT WRIST")
stats_in_paper[3:ncol(stats_in_paper)] = round(stats_in_paper[3:ncol(stats_in_paper)], digits = 2)
write.csv(x = stats_in_paper, file = "reproduce/table/spades_lab_count_stats.v2.csv", quote = FALSE, row.names = FALSE)
