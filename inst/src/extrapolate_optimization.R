rm(list = ls(all.names = TRUE))
require(plyr)
require(dplyr)
require(reshape2)
require(ggplot2)

devices = c('20Hz_2g', '40Hz_2g', '100Hz_2g')

optimize_result = ldply(devices, function(device) {
  file_path = paste0('inst/extdata/extrapolate/optimize_on_', device, '.rds')
  optimize_result = readRDS(file_path)
  optimize_result$device = device
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
    c('device'),
    summarise,
    average_extrapolation_rate = mean(extrapolated_rate),
    sd_extrapolation_rate = sd(extrapolated_rate),
    min_extrapolation_rate = min(extrapolated_rate)
  )

optimize_stats_overall = data.frame(device = "total", sorted_stats[1,3:5], stringsAsFactors = FALSE)
optimize_stats_combined = rbind(optimize_stats_per_device, optimize_stats_overall)

write.csv(x = sorted_stats, file = "inst/table/extrapolation_optimization_stats.csv", quote = FALSE, row.names = FALSE)
write.csv(x = optimize_stats_combined, file = "inst/table/extrapolation_optimal_parameter_stats.csv", quote = FALSE, row.names = FALSE)