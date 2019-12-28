require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(mHealthR)
require(Counts)

filename = "reproduce/extdata/treadmill.rds"
treadmill_data = readRDS(filename)

gt9x_data = treadmill_data %>% filter(ID == 'GT9X') %>% select(-START_TIME, -STOP_TIME, -ID, -GRANGE)

gt9x_counts = gt9x_data %>% ddply(.(MPH, PID, LOCATION), function(chunk){
  gt9x_counts = chunk %>%
    subset(select = 1:4) %>%
    activity_count(
      breaks = '5 sec',
      range = c(-8, 8),
      noise_level = 0.03,
      cutoffs = c(0.2, 2.5),
      aggregation = "axis"
    )
  colnames(gt9x_counts)[2:4] = c('X', 'Y', 'Z')
  result = gt9x_counts %>% cbind(
    chunk %>% subset(select = -(1:4)) %>% unique,
    INDEX = 1:nrow(gt9x_counts),
    stringsAsFactors = FALSE
  )
  return(result)
}, .progress = progress_text())

gt9x_counts_melted = gt9x_counts %>% melt(id=c(1,5,6,7,8,9))
p = ggplot(data = gt9x_counts_melted, aes(x=as.factor(MPH), y=value * 12, fill=LOCATION)) + geom_boxplot() + xlab("Speed (mph)") + ylab("MIMS-unit") + ylim(c(0, 150)) + theme_minimal() + facet_grid(variable ~ .) + ggtitle("cut off at 0.2 and 2.5Hz")
ggsave(filename = "reproduce/figure/treadmill_counts_by_axes_2dot5Hz.png", plot = p, scale = 1, width = 8, height = 6, dpi=100)
