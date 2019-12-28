rm(list = ls(all.names = TRUE))
require(plyr)
require(dplyr)
require(reshape2)
require(ggplot2)

devices = c('20Hz', '30Hz', '40Hz', '50Hz', '60Hz', '70Hz', '80Hz', '90Hz', '100Hz')

optimize_result = ldply(devices, function(device) {
  file_path = paste0('reproduce/extdata/extrapolate/optimize_on_', device, "_2g", '.rds')
  optimize_result = readRDS(file_path)
  optimize_result$device = device
  return(optimize_result)
})

optimize_result$device = factor(optimize_result$device, levels = devices, labels = devices)

cost_function = function(extrapolated_rate, freq, amp, weight = c(0.7, 0.3)){
  freq_thres = 5
  amp_thres = 6
  mask = freq <= freq_thres & amp <= amp_thres
  weighted_extrapolated_rate = (sum(extrapolated_rate[mask] * weight[1]) + sum(extrapolated_rate[!mask] * weight[2]))/2
  return(weighted_extrapolated_rate)
}

optimize_stats = optimize_result %>% ddply(
  c('k', 'spar'),
  summarise,
  average_extrapolation_rate = mean(extrapolated_rate),
  sd_extrapolation_rate = sd(extrapolated_rate),
  min_extrapolation_rate = min(extrapolated_rate),
  max_extrapolation_rate = max(extrapolated_rate),
  weighted_extrapolation_rate = cost_function(extrapolated_rate, freq, amp, c(0.8, 0.2))
)

# the best parameter should be
# 1. No negative extrapolation rate
# 2. As small standard deviation as possible
# 3. As large mean as possible

sorted_stats = optimize_stats %>% arrange(
                                          # desc(weighted_extrapolation_rate)
                                          desc(sign(min_extrapolation_rate)),
                                          desc(average_extrapolation_rate),
                                          # desc(min_extrapolation_rate),
                                          sd_extrapolation_rate
                                          )

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
    c('device'),
    summarise,
    average_extrapolation_rate = mean(extrapolated_rate),
    sd_extrapolation_rate = sd(extrapolated_rate),
    min_extrapolation_rate = min(extrapolated_rate),
    max_extrapolation_rate = max(extrapolated_rate)
  )

optimize_stats_overall = data.frame(device = "average", sorted_stats[1,3:6], stringsAsFactors = FALSE)
optimize_stats_combined = rbind(optimize_stats_per_device, optimize_stats_overall)

write.csv(x = optimize_stats_combined, file = "reproduce/table/extrapolation_optimization_stats.csv", row.names = FALSE, quote = FALSE)

# extrapolation rate vs. devices plot
p1 = ggplot(data = optimize_stats_combined, aes(x = device, y = average_extrapolation_rate)) +
  geom_bar(stat = 'identity', fill = "grey70") +
  geom_point(aes(y = min_extrapolation_rate), shape = 2, size = 2) +
  geom_point(aes(y = max_extrapolation_rate), shape = 6, size = 2) +
  geom_linerange(aes(ymin = min_extrapolation_rate, ymax = average_extrapolation_rate - sd_extrapolation_rate), linetype = "dashed") +
  geom_linerange(aes(ymin = average_extrapolation_rate + sd_extrapolation_rate, ymax = max_extrapolation_rate), linetype = "dashed") +
  geom_errorbar(aes(ymin = average_extrapolation_rate - sd_extrapolation_rate, ymax = average_extrapolation_rate + sd_extrapolation_rate), width = 0.2) +
  ylim(c(-0.1, 1.1)) +
  xlab("Devices (g range: ±2g)") +
  ylab("Extrapolation rate") +
  guides(fill=FALSE) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  theme_bw(base_size = 16, base_family = "Times New Roman")

ggsave(filename = "reproduce/figure/extrapolate_optimize_devices.png", plot = p1, width = 4, height = 2, scale = 1.6)

# extrapolation rate vs. signals under best setting for all devices
p2 = optimize_result %>%
  filter(k == sorted_stats$k[1] & spar == sorted_stats$spar[1]) %>%
  melt(id = c('freq', 'amp', 'sr', 'grange', 'k', 'spar', 'device')) %>%
  ggplot(aes(x = freq, y = amp, fill = value)) +
  geom_raster() +
  scale_fill_gradient2(low = "red", high = "black", mid = "white", midpoint = 0) +
  facet_wrap(variable~device, nrow = 3) +
  xlab("Frequency (Hz)") +
  ylab("Amplitude (g)") +
  ggtitle(sprintf("Smoothing factor = %.2f, neighborhood = %.2f", sorted_stats$spar[1], sorted_stats$k[1])) +
  theme_minimal(base_family = "Times New Roman") +
  theme(title = element_text(size = 9)) +
  theme(strip.background = element_blank(), strip.text.x = element_blank())

p3 = optimize_result %>%
  filter(k == sorted_stats$k[1] & spar == sorted_stats$spar[1] & device %in% c('20Hz', '50Hz', '100Hz')) %>%
  select(-c(test_err,extrapolated_err, sr, grange, k, spar)) %>%
  ggplot(aes(x = freq, y = amp, fill = extrapolated_rate)) +
  geom_raster() +
  scale_fill_gradient2(low = "red", high = "black", mid = "white", midpoint = 0) +
  facet_wrap(~device, ncol = 3) +
  xlab("Frequency (Hz)") +
  ylab("Amplitude (g)") +
  ggtitle(sprintf("Smoothing factor = %.2f, neighborhood = %.2f", sorted_stats$spar[1], sorted_stats$k[1])) +
  theme_minimal(base_family = "Times New Roman") +
  theme(title = element_text(size = 9)) +
  theme(strip.background = element_blank())

# extrapolation rate vs. signals under best setting for selected device
# selected_device = '40Hz_2g'
# p3 = optimize_result %>%
#   filter(device == selected_device & k == sorted_stats$k[1] & spar == sorted_stats$spar[1]) %>%
#   melt(id = c('freq', 'amp', 'sr', 'grange', 'k', 'spar', 'device')) %>%
#   ggplot(aes(x = freq, y = amp, fill = value)) +
#   geom_raster() +
#   scale_fill_gradient2(low = "red", high = "black", mid = "white", midpoint = 0) +
#   facet_grid(.~variable, labeller = function(labels){
#     labels$variable = stringr::str_replace(string = labels$variable, pattern = "_", replacement = " ")
#     labels$variable = R.utils::capitalize(labels$variable)
#     labels$variable[1:2] = paste0(labels$variable[1:2], "or")
#     return(labels)
#   }) +
#   xlab("Frequency (Hz)") +
#   ylab("Amplitude (g)") +
#   ggtitle(sprintf("Smoothing factor = %.2f, neighborhood = %.2f\nSelected device = %s", sorted_stats$spar[1], sorted_stats$k[1], devices[which(selected_device == devices)])) +
#   theme_minimal(base_family = "Times New Roman") +
#   theme(title = element_text(size = 9))
library(extrafont)
loadfonts(device="win")

ggsave(filename = "reproduce/figure/extrapolate_optimize_devices.png", plot = p1, width = 5, height = 2, scale = 1.5, dpi=1500)
ggsave(filename = "reproduce/figure/extrapolate_optimize_devices.eps", plot = p1, width = 5, height = 2, scale = 1.5, dpi=1500)

ggsave(filename = "reproduce/figure/extrapolate_optimal_setting.png", plot = p2, width = 5, height = 2, scale = 1.5, dpi=1500)
ggsave(filename = "reproduce/figure/extrapolate_optimal_setting.eps", plot = p2, width = 5, height = 2, scale = 1.5, dpi=1500)

ggsave(filename = "reproduce/figure/extrapolate_optimal_setting_selected.png", plot = p3, width = 4, height = 1.5, scale = 1.5, dpi=1500)
ggsave(filename = "reproduce/figure/extrapolate_optimal_setting_selected.eps", plot = p3, width = 4, height = 1.5, scale = 1.5, dpi=1500)

write.csv(x = sorted_stats, file = "reproduce/table/extrapolation_optimization_stats.csv", quote = FALSE, row.names = FALSE)
write.csv(x = optimize_stats_combined, file = "reproduce/table/extrapolation_optimal_parameter_stats.csv", quote = FALSE, row.names = FALSE)