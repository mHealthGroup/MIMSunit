input_file = "C:\\Users\\tqshe\\Desktop\\SPIKE_TEST (2017-08-18)RAW.csv"

spike_data = Counts::import_actigraph_csv(input_file)

selected_spike = spike_data[10100:10400,]

write.csv(selected_spike, "C:\\Users\\tqshe\\Desktop\\selected_spike.csv", row.names = FALSE)

p = mHealthR::mhealth.plot_timeseries(list(selected_spike), c("sensor"), list(c(2,3,4)), ncols=1)

p = p + ggplot2::geom_point()

p
