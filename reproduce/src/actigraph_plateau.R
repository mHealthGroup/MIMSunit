require(mhealthformatsupportr)
require(reshape2)
require(plyr)
require(dplyr)
require(ggplot2)

filename = "reproduce/extdata/treadmill1.rds"
walkrun1 = readRDS(filename)
run_beyond_bandwidth = walkrun1 %>% filter(LOCATION == "NondominantWrist" &
                                             SUBJECT == "P2" & MPH == 7.5 & WEIGHTS == "0" & SR == "100")

run_fft = run_beyond_bandwidth[, 1:4] %>% FrequencyResponse.fft(
  Fs = as.numeric(run_beyond_bandwidth$SR[1]),
  normalize = "summed",
  type = "magnitude"
)

filename = "reproduce/extdata/actigraph_response.rds"
actigraph_response = readRDS(filename)

actigraph_response = actigraph_response %>% filter(SR == 100)

new_spectrum = data.frame(spline(
  x = actigraph_response$FREQ,
  y = actigraph_response$VALUE,
  xout = run_fft[, 1]
))
new_spectrum[new_spectrum$x > 10, 2] = NA

fftData = run_fft[-1, ] %>% Magnitude.compute

fftData = fftData %>% mutate(spectrum = new_spectrum[-1, 2])

fftData = fftData[c(1,3,2)]

p = fftData %>% FrequencyResponse.spectrum.ggplot(scale = "normal")

p = p +
  coord_cartesian(xlim = c(0, 10)) +
  scale_color_grey(labels = c("Actigraph proprietary filter response", "running at 7.5mph")) +
  guides(colour = guide_legend(title = "")) +
  theme_bw() +
  ylab("Normalized intensity") +
  xlab("Frequency (Hz)") +
  ggtitle("") +
  theme(legend.position = "bottom")

ggsave(
  path = file.path("reproduce/figure/"),
  filename = "actigraph_plateau.png",
  plot = p,
  scale = 1.5,
  width = 4,
  height = 2
)