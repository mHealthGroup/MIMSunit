# outdoor biking on wrist
startTime = "2015-10-08 14:54:00.000"
endTime = "2015-10-08 14:56:00.000"

file = "offline_data/filter_examples/ActigraphGT9X-AccelerationCalibrated-NA.TAS1E23150066-AccelerationCalibrated.2015-10-08-14-00-00-000-M0400.sensor.csv.gz"

biking = SensorData.importCsv(file) %>% SensorData.clip(startTime = startTime, endTime = endTime)
biking = data.frame(biking, SR = 80, GRANGE = 8, TYPE = "GT9X", ACTIVITY = "Outdoor biking", LOCATION = "NondominantWrist", stringsAsFactors = FALSE)

# walking at 1mph on waist
startTime = "2015-10-08 15:25:54.000"
endTime = "2015-10-08 15:26:54.000"

file = "offline_data/filter_examples/ActigraphGT9X-AccelerationCalibrated-NA.TAS1E23150139-AccelerationCalibrated.2015-10-08-15-00-00-000-M0400.sensor.csv.gz"

walking1mph = SensorData.importCsv(file) %>% SensorData.clip(startTime = startTime, endTime = endTime)
walking1mph = data.frame(walking1mph, SR = 80, GRANGE = 8, TYPE = "GT9X", ACTIVITY = "Walking at 1mph", LOCATION = "Waist",  stringsAsFactors = FALSE)

# running at 5.5mph on ankle
startTime = "2015-10-08 15:34:00.000"
endTime = "2015-10-08 15:36:00.000"

file = "offline_data/filter_examples/ActigraphGT9X-AccelerationCalibrated-NA.TAS1E23150128-AccelerationCalibrated.2015-10-08-15-00-00-000-M0400.sensor.csv.gz"

running5mph = SensorData.importCsv(file) %>% SensorData.clip(startTime = startTime, endTime = endTime)
running5mph = data.frame(running5mph, SR = 80, GRANGE = 8, TYPE = "GT9X", ACTIVITY = "Running at 5.5mph", LOCATION = "DominantAnkle",  stringsAsFactors = FALSE)

filter_examples = rbind(biking, walking1mph, running5mph)

# save as exported data
# devtools::use_data(filter_examples, compress = "bzip2", overwrite = TRUE)
saveRDS(filter_examples, file = "inst/extdata/filter_examples.rds", compress = TRUE)

