require(plyr)
require(dplyr)
require(stringr)
filename = "C:/Users/Qu/Projects/R/Counts/inst/extdata/spades_lab_stats.rds"
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
        SD_AMP = sd(AVERAGE_AMP),
        MIN_AMP = min(AVERAGE_AMP),
        MEAN_AMP = mean(AVERAGE_AMP),
        MEDIAN_AMP = median(AVERAGE_AMP),
        MAX_AMP = max(AVERAGE_AMP),
        SD_DOMINANT_FREQ = sd(AVERAGE_DOMINANT_FREQ),
        MIN_DOMINANT_FREQ = min(AVERAGE_DOMINANT_FREQ),
        MEAN_DOMINANT_FREQ = mean(AVERAGE_DOMINANT_FREQ),
        MEDIAN_DOMINANT_FREQ = median(AVERAGE_DOMINANT_FREQ),
        MAX_DOMINANT_FREQ = max(AVERAGE_DOMINANT_FREQ),
        SD_DOMINANT_FREQ_SECOND = sd(AVERAGE_DOMINANT_FREQ_SECOND),
        MIN_DOMINANT_FREQ_SECOND = min(AVERAGE_DOMINANT_FREQ_SECOND),
        MEAN_DOMINANT_FREQ_SECOND = mean(AVERAGE_DOMINANT_FREQ_SECOND),
        MEDIAN_DOMINANT_FREQ_SECOND = median(AVERAGE_DOMINANT_FREQ_SECOND),
        MAX_DOMINANT_FREQ_SECOND = max(AVERAGE_DOMINANT_FREQ_SECOND),
        SD_DOMINANT_FREQ_THIRD = sd(AVERAGE_DOMINANT_FREQ_THIRD),
        MIN_DOMINANT_FREQ_THIRD = min(AVERAGE_DOMINANT_FREQ_THIRD),
        MEAN_DOMINANT_FREQ_THIRD = mean(AVERAGE_DOMINANT_FREQ_THIRD),
        MEDIAN_DOMINANT_FREQ_THIRD = median(AVERAGE_DOMINANT_FREQ_THIRD),
        MAX_DOMINANT_FREQ_THIRD = max(AVERAGE_DOMINANT_FREQ_THIRD)
        )

stats_in_paper = spades_lab_stats %>% filter(LOCATION == "DOMINANT WAIST" | LOCATION == "NON DOMINANT WRIST")
stats_in_paper[3:ncol(stats_in_paper)] = round(stats_in_paper[3:ncol(stats_in_paper)], digits = 2)
write.csv(x = stats_in_paper, file = "inst/table/spades_lab_count_stats.csv", quote = FALSE, row.names = FALSE)
