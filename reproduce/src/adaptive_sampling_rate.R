require(mhealthformatsupportr)
require(plyr)
require(dplyr)
require(ggplot2)

filename = "reproduce/extdata/adaptive_sampling_rate.rds"
adaptive_sr = readRDS(filename)

plotData = MIMSunit::clip_data(
  adaptive_sr,
  start_time = adaptive_sr[1, 1] + 400,
  stop_time = adaptive_sr[1, 1] + 800
)

names(plotData) = c("t", "sr")

plotData[, 1] = as.numeric(plotData[, 1] - plotData[1, 1])

plotData = plotData %>% mutate(g50 = as.numeric(sr > 50) - as.numeric(sr < 47))

p = plotData %>% ggplot(aes(
  x = t,
  ymax = sr,
  ymin = 0,
  y = sr
)) %+%
  geom_linerange(aes(color = as.factor(g50)), lwd = 0.6) +
  # geom_point(aes(shape = as.factor(g50))) +
  ylab("sampling frequency (Hz)") +
  xlab("time (s)") +
  guides(colour = guide_legend(title = "")) +
  scale_color_grey(labels = c("Data flushing", "Interaction off", "Interaction on")) +
  theme_bw() +
  theme(legend.position = "bottom", plot.background = element_rect(fill = "white"))

ggsave(
  path = file.path("reproduce/figure/"),
  filename = "adaptive_sampling_rate.png",
  plot = p,
  scale = 1.5,
  width = 4,
  height = 2
)
