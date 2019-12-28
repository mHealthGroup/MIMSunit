require(mhealthformatsupportr)
require(reshape2)
require(plyr)
require(dplyr)
require(ggplot2)

filename = "reproduce/extdata/running_maxed_out.rds"
running_maxed_out = readRDS(filename)
# problem 1 maxed out problem
startTime = running_maxed_out[1,1]
clipped_running = ddply(running_maxed_out, .variables = "TYPE", function(data){
  return(data %>% SensorData.clip(startTime = startTime + 20, endTime = startTime + 25))
})
clipped_running = clipped_running[-c(3,4)]
clipped_running = clipped_running %>% dplyr::filter(TYPE == "GT3X" | TYPE == "GT9X")
label_data = data.frame(x = c(4.5, 4.2), y = c(3.5, 5.5), label = unique(clipped_running$ACTIGRAPH_COUNT_X), color = c("GT3X", "GT9X"))
melted = melt(clipped_running, id = c(1,3,4,5,6,7), factorsAsStrings = TRUE)
melted$HEADER_TIME_STAMP = as.numeric(melted$HEADER_TIME_STAMP - melted$HEADER_TIME_STAMP[1])
p = ggplot() +
  geom_line(data = melted, aes(x = HEADER_TIME_STAMP, y = value, colour = TYPE)) +
  scale_color_grey(labels = c("GT3X (30Hz, 3g)", "GT9X (100Hz, 16g)")) +
  guides(colour = guide_legend(title = "")) +
  geom_label(data = label_data, aes(x = x, y = y, label = label, color = color), size = 4) +

  geom_hline(yintercept = 3, lty = "dashed", colour = "gray") +
  annotate("text", 0.5, 3.5, label = "GT3X dynamic range at 3g", colour = "gray") +


  theme_bw() +
  xlab("time (s)") +
  ylab("X-axis acceleration (g)") +
  theme(legend.position = "bottom")


ggsave(path = file.path("reproduce/figure/"), filename = "actigraph_maxed_out.png", plot = p, scale = 1.5, width = 4, height = 2)