library(h5)
input_data = "F:\\data\\spades_lab_cache3\\SPADES_1\\Derived\\TAS1E23150108_dominant wrist_merged.sensor.h5"
data <- h5file(input_data, "r")
values = data['TAS1E23150108_dominant_wrist/block0_values'][,1:3]
ts = data['TAS1E23150108_dominant_wrist/block1_values'][]/1000000000
ts = as.POSIXct(ts, origin="1970-01-01", tz = "UTC")

test_data = data.frame(values)
test_data["HEADER_TIME_STAMP"] = ts
colnames(test_data)[1:3] = c("X", "Y", "Z")
test_data = test_data[c("HEADER_TIME_STAMP", 'X', 'Y', 'Z')]
count_values_axes = Counts::activity_count(test_data, breaks = "1 min", range = c(-8, 8), noise_level = 0.03, aggregation = "axis")
count_values_sum = Counts::sumUp(count_values_axes)
count_values_vm = Counts::magnitude(count_values_axes)
count_values_vm_after_extrapolation = Counts::activity_count(test_data, breaks = "1 min", range = c(-8, 8), noise_level = 0.03, aggregation = "vm", aggregate_after_extrapolation = TRUE)
plot_data = count_values_sum
plot_data['vm'] = count_values_vm[,2]
plot_data['vm_after_extrapolation'] = count_values_vm_after_extrapolation[,2]
colnames(plot_data)[2] = c('sum')

library(dplyr)
library(ggplot2)
library(reshape2)

write.csv(x = plot_data, file = "inst/table/test_aggregation.csv", quote = FALSE, row.names = FALSE)
plot_data = plot_data %>% melt(id=c('HEADER_TIME_STAMP'))
ggplot(data=plot_data, aes(x=HEADER_TIME_STAMP, y=value, color=variable)) + geom_line()
