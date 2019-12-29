require(ggplot2)
require(dplyr)
require(caTools)
filename = "reproduce/extdata/filter_examples.rds"
filter_examples = readRDS(filename)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

colors = gg_color_hue(2)

# compare between butterworth and elliptic
example_butter_vs_elliptic = function(){
biking_data = filter_examples %>% dplyr::filter(ACTIVITY == "Outdoor biking")
biking = biking_data[, c(1, 3)]
butter_biking = SensorData.filter.iir(
  biking,
  Fs = 80,
  Fc = c(0.2, 5),
  order = 4,
  filter = "butter",
  type = "pass"
)[[1]]
ellip_biking = SensorData.filter.iir(
  biking,
  Fs = 80,
  Fc = c(0.2, 5),
  order = 4,
  filter = "ellip",
  type = "pass"
)[[1]]

names(butter_biking) = names(biking)
names(ellip_biking) = names(biking)

st = biking[1, 1] + 20
biking = SensorData.clip(biking, startTime = biking[1, 1], endTime = biking[1, 1] + 15)
butter_biking = SensorData.clip(butter_biking, startTime = biking[1, 1], endTime = biking[1, 1] + 15)
ellip_biking = SensorData.clip(ellip_biking, startTime = biking[1, 1], endTime = biking[1, 1] + 15)
biking = data.frame(biking, group = "original")
butter_biking = data.frame(butter_biking, group = "butterworth")
ellip_biking = data.frame(ellip_biking, group = "elliptic")

biking_forplot = rbind(biking, butter_biking, ellip_biking)

p = ggplot(
  data = biking_forplot,
  aes(x = HEADER_TIME_STAMP, y = Y_ACCELERATION_METERS_PER_SECOND_SQUARED, colour = group)
) + geom_line(lwd = 1) +
  scale_color_manual(
    values = c("gray", colors[1], colors[2]),
    labels = c(
      "Original signal",
      "Filtered by butterworth(0.2~5)",
      "Filtered by elliptic (0.2~5)"
    )
  ) + xlab("Time (s)") + ylab("X-axis Acceleration (g)") + theme_minimal(base_size = 16) + guides(colour = guide_legend(title = "")) + theme(legend.position = "bottom")


butter_biking_fft = butter_biking[, 1:2] %>% FrequencyResponse.fft(Fs = 80,
                                                                   normalize = "normalized",
                                                                   type = "magnitude")
ellip_biking_fft = ellip_biking[, 1:2] %>% FrequencyResponse.fft(Fs = 80,
                                                                 normalize = "normalized",
                                                                 type = "magnitude")
biking_fft = biking[, 1:2] %>% FrequencyResponse.fft(Fs = 80,
                                                     normalize = "normalized",
                                                     type = "magnitude")

biking_fft_forplot = data.frame(
  HEADER_FREQUENCY_STAMP = biking_fft[, 1],
  original = biking_fft[, 2],
  butter = butter_biking_fft[, 2],
  ellip = ellip_biking_fft[, 2]
)

p2 = biking_fft_forplot %>% FrequencyResponse.spectrum.ggplot(scale = "db")

p2 = p2 +
  guides(colour = guide_legend(title = "")) +
  theme_minimal(base_size = 16) +
  ylab("intensity (dB)") +
  xlab("frequency (log(Hz))") + ggtitle("") +
  theme(legend.position = "bottom") +
  theme_minimal(base_size = 16) +
  scale_color_manual(
  values = c("gray", colors[1], colors[2]),
  labels = c(
    "Original signal",
    "Filtered by butterworth(0.2~5)",
    "Filtered by elliptic (0.2~5)"
  )
) +
  guides(colour = guide_legend(title = "")) +
  theme(legend.position = "bottom") +
  scale_x_log10(breaks = c(0.1, 1, 2.5, 4, 5, 7, 10, 20, 35, 50), limits = c(0.1, 50))

ggsave(
  path = file.path("reproduce/figure/"),
  filename = "filter_example_butter_vs_elliptic_time.png",
  plot = p,
  scale = 2,
  width = 4,
  height = 2
)
ggsave(
  path = file.path("reproduce/figure/"),
  filename = "filter_example_butter_vs_elliptic_freq.png",
  plot = p2,
  scale = 2,
  width = 4,
  height = 2
)
}

# example_butter_vs_elliptic()

# justify higher end cut off

example_higher_cutoff = function(){
running_data = filter_examples %>% dplyr::filter(ACTIVITY == "Running at 5.5mph")
running = running_data[, c(1, 2)]
butter_running = Counts::iir(
  running,
  Fs = 80,
  Fc = c(0.2, 5),
  order = 4,
  filter_type = "butter",
  type = "pass"
)
butter_narrow_running = Counts::iir(
  running,
  Fs = 80,
  Fc = c(0.25, 2.5),
  order = 4,
  filter_type = "butter",
  type = "pass"
)

names(butter_running) = names(running)
names(butter_narrow_running) = names(running)

st = running[1, 1] + 20
running = MIMSunit::clip_data(running, start_time = running[1, 1], stop_time = running[1, 1] + 15)
butter_running = MIMSunit::clip_data(butter_running,
                                 start_time = running[1, 1],
                                 stop_time = running[1, 1] + 15,
                                 file_type = "sensor")
butter_narrow_running = MIMSunit::clip_data(butter_narrow_running,
                                        start_time = running[1, 1],
                                        stop_time = running[1, 1] + 15,
                                        file_type = "sensor")
running = data.frame(running, group = "original")
butter_running = data.frame(butter_running, group = "butterworth")
butter_narrow_running = data.frame(butter_narrow_running, group = "butter_narrowtic")

auc_running = trapz(butter_running[[1]], abs(butter_running[[2]]))
auc_narrow_running = trapz(butter_narrow_running[[1]], abs(butter_narrow_running[[2]]))

print(auc_running)
print(auc_narrow_running)

print(abs(auc_narrow_running - auc_running)/auc_running)

running_forplot = rbind(running, butter_running, butter_narrow_running)

p = ggplot(
  data = running_forplot,
  aes(x = HEADER_TIME_STAMP, y = X_ACCELERATION_METERS_PER_SECOND_SQUARED, colour = group)
) + geom_line(lwd = 1) +
  scale_color_manual(
    values = c("gray", colors[1], colors[2]),
    labels = c(
      "Original signal",
      "Filtered by butterworth(0.2~5)",
      "Filtered by butterworth (0.25~2.5)"
    )
  ) + xlab("Time (s)") + ylab("X-axis Acceleration (g)") + theme_minimal(base_size = 16) + guides(colour = guide_legend(title = "")) + theme(legend.position = "bottom")


butter_running_fft = butter_running[, 1:2] %>% FrequencyResponse.fft(Fs = 80,
                                                                     normalize = "normalized",
                                                                     type = "magnitude")
butter_narrow_running_fft = butter_narrow_running[, 1:2] %>% FrequencyResponse.fft(Fs = 80,
                                                                                   normalize = "normalized",
                                                                                   type = "magnitude")
running_fft = running[, 1:2] %>% FrequencyResponse.fft(Fs = 80,
                                                       normalize = "normalized",
                                                       type = "magnitude")

running_fft_forplot = data.frame(
  HEADER_FREQUENCY_STAMP = running_fft[, 1],
  original = running_fft[, 2],
  butter = butter_running_fft[, 2],
  butter_narrow = butter_narrow_running_fft[, 2]
)

p2 = running_fft_forplot %>% FrequencyResponse.spectrum.ggplot(scale = "db")

p2 = p2 + guides(colour = guide_legend(title = "")) +
  theme_minimal(base_size = 16) +
  ylab("intensity (dB)") +
  xlab("frequency (log(Hz))") +
  ggtitle("") +
  theme(legend.position = "bottom") +
  theme_minimal(base_size = 16) +
  scale_color_manual(
  values = c("gray", colors[1], colors[2]),
  labels = c(
    "Original signal",
    "Filtered by butterworth(0.2~5)",
    "Filtered by butterworth (0.25~2.5)"
  )
) +
  guides(colour = guide_legend(title = "")) +
  theme(legend.position = "bottom") +
  scale_x_log10(breaks = c(0.1, 1, 2.5, 4, 5, 7, 10, 20, 35, 50),limits = c(0.1, 50))

ggsave(
  path = file.path("reproduce/figure/"),
  filename = "filter_example_higher_cutoff_time.png",
  plot = p,
  scale = 2,
  width = 4,
  height = 2
)
ggsave(
  path = file.path("reproduce/figure/"),
  filename = "filter_example_higher_cutoff_freq.png",
  plot = p2,
  scale = 2,
  width = 4,
  height = 2
)
}
# example_higher_cutoff()

# walking at 1mph
example_lower_cutoff = function(){
walking_data = filter_examples %>% dplyr::filter(ACTIVITY == "Walking at 1mph")
walking = walking_data[, c(1, 4)]
butter_walking = Counts::iir(
  walking,
  Fs = 80,
  Fc = c(0.2, 5),
  order = 4,
  filter = "butter",
  type = "pass"
)
butter_high_walking = Counts::iir(
  walking,
  Fs = 80,
  Fc = c(0.6, 5),
  order = 4,
  filter = "butter",
  type = "pass"
)

names(butter_walking) = names(walking)
names(butter_high_walking) = names(walking)

st = walking[1, 1] + 20
walking = MIMSunit::clip_data(walking, start_time = walking[1, 1], stop_time = walking[1, 1] + 15)
butter_walking = MIMSunit::clip_data(butter_walking,
                                 start_time = walking[1, 1],
                                 stop_time = walking[1, 1] + 15,
                                 file_type = "sensor")
butter_high_walking = MIMSunit::clip_data(butter_high_walking,
                                      start_time = walking[1, 1],
                                      stop_time = walking[1, 1] + 15,
                                      file_type = "sensor")
walking = data.frame(walking, group = "original")
butter_walking = data.frame(butter_walking, group = "butterworth")
butter_high_walking = data.frame(butter_high_walking, group = "butterworth_high")

auc_walking = trapz(butter_walking[[1]], abs(butter_walking[[2]]))
auc_high_walking = trapz(butter_high_walking[[1]], abs(butter_high_walking[[2]]))

print(auc_walking)
print(auc_high_walking)

print(abs(auc_high_walking - auc_walking)/auc_walking)

walking_forplot = rbind(walking, butter_walking, butter_high_walking)

p = ggplot(
  data = walking_forplot,
  aes(x = HEADER_TIME_STAMP, y = Z_ACCELERATION_METERS_PER_SECOND_SQUARED, colour = group)
) + geom_line(lwd = 1) +
  scale_color_manual(
    values = c("gray", colors[1], colors[2]),
    labels = c(
      "Original signal",
      "Filtered by butterworth(0.2~5)",
      "Filtered by butterworth (0.6~5)"
    )
  ) + xlab("Time (s)") + ylab("X-axis Acceleration (g)") + theme_minimal(base_size = 16) + guides(colour = guide_legend(title = "")) + theme(legend.position = "bottom")


butter_walking_fft = butter_walking[, 1:2] %>% FrequencyResponse.fft(Fs = 80,
                                                                     normalize = "normalized",
                                                                     type = "magnitude")
butter_high_walking_fft = butter_high_walking[, 1:2] %>% FrequencyResponse.fft(Fs = 80,
                                                                               normalize = "normalized",
                                                                               type = "magnitude")
walking_fft = walking[, 1:2] %>% FrequencyResponse.fft(Fs = 80,
                                                       normalize = "normalized",
                                                       type = "magnitude")

walking_fft_forplot = data.frame(
  HEADER_FREQUENCY_STAMP = walking_fft[, 1],
  original = walking_fft[, 2],
  butter = butter_walking_fft[, 2],
  butter_high = butter_high_walking_fft[, 2]
)

p2 = walking_fft_forplot %>% FrequencyResponse.spectrum.ggplot(scale = "db")

p2 = p2 + guides(colour = guide_legend(title = "")) + theme_minimal(base_size = 16) + ylab("intensity (dB)") + xlab("frequency (log(Hz))") + ggtitle("") + theme(legend.position = "bottom") + theme_minimal(base_size = 16) + scale_color_manual(
  values = c("gray", colors[1], colors[2]),
  labels = c(
    "Original signal",
    "Filtered by butterworth(0.2~5)",
    "Filtered by butterworth (0.6~5)"
  )
) + guides(colour = guide_legend(title = "")) + theme(legend.position = "bottom") + scale_x_log10(breaks = c(0.1, 1, 2.5, 4, 5, 7, 10, 20, 35, 50),
                                                                                                  limits = c(0.1, 50))

ggsave(
  path = file.path("reproduce/figure/"),
  filename = "filter_example_lower_cutoff_time.png",
  plot = p,
  scale = 2,
  width = 4,
  height = 2
)
ggsave(
  path = file.path("reproduce/figure/"),
  filename = "filter_example_lower_cutoff_freq.png",
  plot = p2,
  scale = 2,
  width = 4,
  height = 2
)
}
example_lower_cutoff()
