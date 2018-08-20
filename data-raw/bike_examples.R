# outdoor biking on wrist
outdoor_bike_start_time = "2015-10-08 14:54:00.000"
outdoor_bike_end_time = "2015-10-08 14:56:00.000"
indoor_bike_start_time = "2015-10-08 15:23:30.000"
indoor_bike_end_time = "2015-10-08 15:24:30.000"

outdoor_wrist_file = "D:/data/spades_lab/SPADES_2/MasterSynced/2015/10/08/14/ActigraphGT9X-AccelerationCalibrated-NA.TAS1E23150066-AccelerationCalibrated.2015-10-08-14-00-00-000-M0400.sensor.csv"
outdoor_hip_file = "D:/data/spades_lab/SPADES_2/MasterSynced/2015/10/08/14/ActigraphGT9X-AccelerationCalibrated-NA.TAS1E23150162-AccelerationCalibrated.2015-10-08-14-00-00-000-M0400.sensor.csv"
indoor_wrist_file = "D:/data/spades_lab/SPADES_2/MasterSynced/2015/10/08/15/ActigraphGT9X-AccelerationCalibrated-NA.TAS1E23150066-AccelerationCalibrated.2015-10-08-15-00-00-000-M0400.sensor.csv"
indoor_hip_file = "D:/data/spades_lab/SPADES_2/MasterSynced/2015/10/08/15/ActigraphGT9X-AccelerationCalibrated-NA.TAS1E23150162-AccelerationCalibrated.2015-10-08-15-00-00-000-M0400.sensor.csv"

outdoor_wrist_biking = mHealthR::mhealth.read(outdoor_wrist_file, filetype = 'sensor') %>% mHealthR::mhealth.clip(start_time = outdoor_bike_start_time, stop_time = outdoor_bike_end_time, file_type = 'sensor')
outdoor_wrist_biking = data.frame(outdoor_wrist_biking, SR = 80, GRANGE = 8, TYPE = "GT9X", ACTIVITY = "Bike -Outdoor", LOCATION = "Non-dominant wrist", stringsAsFactors = FALSE)
indoor_wrist_biking = mHealthR::mhealth.read(indoor_wrist_file, filetype = 'sensor') %>% mHealthR::mhealth.clip(start_time = indoor_bike_start_time, stop_time = indoor_bike_end_time, file_type = 'sensor')
indoor_wrist_biking = data.frame(indoor_wrist_biking, SR = 80, GRANGE = 8, TYPE = "GT9X", ACTIVITY = "Bike -Erg.", LOCATION = "Non-dominant wrist", stringsAsFactors = FALSE)
outdoor_hip_biking = mHealthR::mhealth.read(outdoor_hip_file, filetype = 'sensor') %>% mHealthR::mhealth.clip(start_time = outdoor_bike_start_time, stop_time = outdoor_bike_end_time, file_type = 'sensor')
outdoor_hip_biking = data.frame(outdoor_hip_biking, SR = 80, GRANGE = 8, TYPE = "GT9X", ACTIVITY = "Bike -Outdoor", LOCATION = "Non-dominant hip", stringsAsFactors = FALSE)
indoor_hip_biking = mHealthR::mhealth.read(indoor_hip_file, filetype = 'sensor') %>% mHealthR::mhealth.clip(start_time = indoor_bike_start_time, stop_time = indoor_bike_end_time, file_type = 'sensor')
indoor_hip_biking = data.frame(indoor_hip_biking, SR = 80, GRANGE = 8, TYPE = "GT9X", ACTIVITY = "Bike -Erg.", LOCATION = "Non-dominant hip", stringsAsFactors = FALSE)


biking_examples = rbind(outdoor_wrist_biking, indoor_wrist_biking, outdoor_hip_biking, indoor_hip_biking)

# save as exported data
saveRDS(biking_examples, file = "inst/extdata/biking_examples.rds", compress = TRUE)

