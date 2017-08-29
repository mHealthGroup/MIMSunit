require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(mHealthR)
require(Counts)
filename = "inst/extdata/treadmill.rds"
treadmill_data = readRDS(filename)

selected_data = treadmill_data %>% filter(GRANGE == "8", SR == 100, ID == "GT9X") %>% select(-START_TIME, -STOP_TIME, -SR, -GRANGE, -ID)

summary_data = mhealth.extract_characteristics(selected_data, "sensor", c(2,3,4), group_cols = c(5, 6, 7), preset = "primary")

dominant_freq_summary = summary_data %>% group_by(MPH, COLUMNS, LOCATION) %>% summarise(
  Mean_dominant_freq = format(mean(DOMINANT_FREQ), digits=3),
  Sd_dominant_freq = format(sd(DOMINANT_FREQ), digits=3)
)

p = ggplot(data = summary_data, aes(x=as.character(MPH), y=DOMINANT_FREQ, fill=LOCATION)) + geom_boxplot(outlier.shape=NA)+ xlab("Speed (mph)") + ylab("Dominant frequency (Hz)") + theme_minimal() + facet_grid(COLUMNS ~ .)

ggsave(filename = "inst/figure/treadmill_dominant_freqs.png", plot = p, scale = 1, width = 8, height = 6, dpi = 100)

write.csv(x = dominant_freq_summary, file = "inst/table/treadmill_dominant_freqs.csv", quote = FALSE, row.names = FALSE, )
