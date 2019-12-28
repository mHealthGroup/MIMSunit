# library(h5)
# require(Counts)
# require(mHealthR)
# input_data = "F:\\data\\spades_lab_cache3\\SPADES_1\\Derived\\TAS1E23150108_dominant wrist_merged.sensor.h5"
# data <- h5file(input_data, "r")
# values = data['TAS1E23150108_dominant_wrist/block0_values'][,1:3]
# ts = data['TAS1E23150108_dominant_wrist/block1_values'][]/1000000000
# ts = as.POSIXct(ts, origin="1970-01-01", tz = "UTC")
# test_data = data.frame(values)
# test_data["HEADER_TIME_STAMP"] = ts
# colnames(test_data)[1:3] = c("X", "Y", "Z")
# test_data = test_data[c("HEADER_TIME_STAMP", 'X', 'Y', 'Z')]
#
# input_annotation = "F:\\data\\spades_lab_cache3\\SPADES_1\\Derived\\SPADESInLab_merged.annotation.csv"
# annotation = mhealth.read(input_annotation, filetype = "annotation")
# annotation$START_TIME = lubridate::force_tz(annotation$START_TIME, "UTC")
# annotation$STOP_TIME = lubridate::force_tz(annotation$STOP_TIME, "UTC")
# annotation$HEADER_TIME_STAMP = lubridate::force_tz(annotation$HEADER_TIME_STAMP, "UTC")
#
# count_values_axes = Counts::activity_count(test_data, breaks = "1 min", range = c(-8, 8), noise_level = 0.03, aggregation = "axis")
# count_values_sum = Counts::sumUp(count_values_axes)
# count_values_vm = Counts::magnitude(count_values_axes)
# count_values_vm_after_extrapolation = Counts::activity_count(test_data, breaks = "1 min", range = c(-8, 8), noise_level = 0.03, aggregation = "vm", aggregate_after_extrapolation = TRUE)
# plot_data = count_values_sum
# plot_data['vm'] = count_values_vm[,2]
# plot_data['vm_after_extrapolation'] = count_values_vm_after_extrapolation[,2]
# colnames(plot_data)[2] = c('sum')
# plot_data = na.omit(plot_data)
#
# model_vm = lm(formula = sum ~ vm, data = plot_data)
# model_vm2 = lm(formula = sum ~ vm_after_extrapolation, data = plot_data)
# plot_data$scaled_vm = plot_data$vm * model_vm$coefficients[2] + model_vm$coefficients[1]
# plot_data$scaled_vm_after_extrapolation = plot_data$vm_after_extrapolation * model_vm2$coefficients[2] + model_vm2$coefficients[1]
library(dplyr)
library(ggplot2)
library(reshape2)


p = mhealth.plot_timeseries(list(annotation), c('annotation'), select_cols = list(c(4)), text_annotation = TRUE) + scale_color_discrete(guide=FALSE)

plot_data_melted = plot_data %>% select(-vm, -vm_after_extrapolation) %>% melt(id=c('HEADER_TIME_STAMP'))
p = p + geom_line(data=plot_data_melted, aes(x=HEADER_TIME_STAMP, y=value+15, lty=variable), colour='gray30', size=1) + theme(legend.position = c(0.15, 0.85), legend.title = element_blank())
ggsave(filename = "reproduce/figure/combination_comparison.png", plot = p, scale = 1, width = 8, height = 6, dpi = 1000)
write.csv(x = plot_data, file = "reproduce/table/test_aggregation.csv", quote = FALSE, row.names = FALSE)
plot_data_melted = plot_data %>% melt(id=c('HEADER_TIME_STAMP', 'vm', 'vm_after_extrapolation'))
p1 = ggplot(data=plot_data_melted, aes(x=HEADER_TIME_STAMP, y=value, color=variable)) + geom_line() + theme_minimal() + theme(legend.position="bottom")
p2 = ggplot(data=plot_data, aes(x=sum, y=vm)) + geom_point() + geom_point(data=plot_data, aes(x=sum, y=vm_after_extrapolation), color='red') + xlab('MIMS-unit (sum)') + ylab('MIMS-unit (vm)')
