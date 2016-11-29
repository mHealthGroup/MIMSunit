rm(list = ls(all.names = TRUE))
require(plyr)
require(dplyr)
require(reshape2)
require(ggplot2)

devices = c('20Hz_2g', '40Hz_2g', '100Hz_2g')
device_names = c('Actival3', "Recommended", "LG urbane R")

optimize_result = ldply(devices, function(device) {
  file_path = paste0('inst/extdata/extrapolate/optimize_on_', device, '.rds')
  optimize_result = readRDS(file_path)
  optimize_result$device = device
  optimize_result$device_name = device_names[which(devices == device)]
  return(optimize_result)
})

optimize_stats = optimize_result %>% ddply(
  c('k', 'spar'),
  summarise,
  average_extrapolation_rate = mean(extrapolated_rate),
  sd_extrapolation_rate = sd(extrapolated_rate),
  min_extrapolation_rate = min(extrapolated_rate)
)

# the best parameter should be
# 1. No negative extrapolation rate
# 2. As small standard deviation as possible
# 3. As large mean as possible

sorted_stats = optimize_stats %>% arrange(desc(sign(min_extrapolation_rate)),
                                          desc(average_extrapolation_rate),
                                          sd_extrapolation_rate)

message(sprintf("Among devices %s", paste(devices, collapse = ", ")))
message(
  sprintf(
    "Best parameter combination is: k = %.2f, spar = %.2f.",
    sorted_stats$k[1],
    sorted_stats$spar[1]
  )
)
message(
  sprintf(
    "Average extrapolation rate = %.2f\nStandard deviation of extrapolation rate = %.2f\nMinimum extrapolation rate = %.2f",
    sorted_stats$average_extrapolation_rate[1],
    sorted_stats$sd_extrapolation_rate[1],
    sorted_stats$min_extrapolation_rate[1]
  )
)

optimize_stats_per_device = optimize_result %>%
  dplyr::filter(k == sorted_stats$k[1] & spar == sorted_stats$spar[1]) %>%
  ddply(
    c('device', 'device_name'),
    summarise,
    average_extrapolation_rate = mean(extrapolated_rate),
    sd_extrapolation_rate = sd(extrapolated_rate),
    min_extrapolation_rate = min(extrapolated_rate)
  )

optimize_stats_overall = data.frame(device_name = "total", device = "total", sorted_stats[1,3:5], stringsAsFactors = FALSE)
optimize_stats_combined = rbind(optimize_stats_per_device, optimize_stats_overall)

# extrapolation rate vs. devices plot
p1 = ggplot(data = optimize_stats_combined, aes(x = device_name, y = average_extrapolation_rate, fill = device_name)) +
  geom_bar(stat = 'identity') +
  geom_point(aes(y = min_extrapolation_rate), shape = 17, size = 2) +
  geom_linerange(aes(ymin = min_extrapolation_rate, ymax = average_extrapolation_rate - sd_extrapolation_rate), linetype = "dashed") +
  geom_errorbar(aes(ymin = average_extrapolation_rate - sd_extrapolation_rate, ymax = average_extrapolation_rate + sd_extrapolation_rate), width = 0.2) +
  ylim(c(0, 1)) +
  xlab("Devices") +
  ylab("Extrapolation rate") +
  theme_bw()

# extrapolation rate vs. signals under best setting for all devices
p2 = optimize_result %>%
  filter(k == sorted_stats$k[1] & spar == sorted_stats$spar[1]) %>%
  melt(id = c('freq', 'amp', 'sr', 'grange', 'k', 'spar', 'device', 'device_name')) %>%
  ggplot(aes(x = freq, y = amp, fill = value)) +
  geom_raster() +
  scale_fill_gradient(low = "black", high = "white") +
  facet_wrap(device_name~variable, labeller = function(labels){
    labels$variable = stringr::str_replace(string = labels$variable, pattern = "_", replacement = " ")
    labels$variable = R.utils::capitalize(labels$variable)
    labels$variable[1:2] = paste0(labels$variable[1:2], "or")
    return(labels)
    }) +
  xlab("Frequency (Hz)") +
  ylab("Amplitude (g)") +
  ggtitle(sprintf("Smoothing factor = %.2f, neighborhood = %.2f", sorted_stats$spar[1], sorted_stats$k[1])) +
  theme_minimal() +
  theme(title = element_text(size = 9))

# extrapolation rate vs. signals under best setting for selected device
selected_device = '40Hz_2g'
p3 = optimize_result %>%
  filter(device == selected_device & k == sorted_stats$k[1] & spar == sorted_stats$spar[1]) %>%
  melt(id = c('freq', 'amp', 'sr', 'grange', 'k', 'spar', 'device', 'device_name')) %>%
  ggplot(aes(x = freq, y = amp, fill = value)) +
  geom_raster() +
  scale_fill_gradient(low = "black", high = "white") +
  facet_grid(.~variable, labeller = function(labels){
    labels$variable = stringr::str_replace(string = labels$variable, pattern = "_", replacement = " ")
    labels$variable = R.utils::capitalize(labels$variable)
    labels$variable[1:2] = paste0(labels$variable[1:2], "or")
    return(labels)
  }) +
  xlab("Frequency (Hz)") +
  ylab("Amplitude (g)") +
  ggtitle(sprintf("Smoothing factor = %.2f, neighborhood = %.2f\nSelected device = %s", sorted_stats$spar[1], sorted_stats$k[1], device_names[which(selected_device == devices)])) +
  theme_minimal() +
  theme(title = element_text(size = 9))

ggsave(filename = "inst/figure/extrapolate_optimize_devices.png", plot = p1, width = 4, height = 3, scale = 1.5)

ggsave(filename = "inst/figure/extrapolate_optimal_setting.png", plot = p2, width = 4, height = 4.5, scale = 1.5)

ggsave(filename = "inst/figure/extrapolate_optimal_setting_selected.png", plot = p3, width = 4, height = 1.5, scale = 1.5)

write.csv(x = sorted_stats, file = "inst/table/extrapolation_optimization_stats.csv", quote = FALSE, row.names = FALSE)
write.csv(x = optimize_stats_combined, file = "inst/table/extrapolation_optimal_parameter_stats.csv", quote = FALSE, row.names = FALSE)