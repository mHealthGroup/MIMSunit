startTime = as.POSIXct("2015-10-08 14:12:15.000")
endTime = as.POSIXct("2015-10-08 14:13:30.000")

jjData = SensorData.importCsv("offline_data/jumping_jack_maxed_out/ActigraphGT9X-AccelerationCalibrated-NA.TAS1E23150066-AccelerationCalibrated.2015-10-08-14-00-00-000-M0400.sensor.csv.gz")

jjData_clip = SensorData.clip(jjData, startTime = startTime + 5, endTime = startTime + 15)

jjData_crop = jjData_clip[,2:4] %>% adply(.margins = 2, function(colData){
  selection = sum(colData > 3)
  colData[colData > 3] = 3 + rnorm(selection, 0, 0.05)
  selection = sum(colData < -3)
  colData[colData < -3] = -3 + rnorm(selection, 0, 0.05)
  return(colData[[1]])
}, .id = NULL)

jjData_crop = data.frame(t(jjData_crop), row.names = NULL)
jjData_crop = cbind(jjData_clip[[1]], jjData_crop)
names(jjData_crop) = names(jjData)

jumping_jack_maxed_out = rbind(cbind(jjData_clip, GRANGE = 8), cbind(jjData_crop, GRANGE = 3))
jumping_jack_maxed_out$SR = 80
jumping_jack_maxed_out$TYPE = "GT9X"
jumping_jack_maxed_out$LOCATION = "NondominantWrist"

# devtools::use_data(jumping_jack_maxed_out, compress = "bzip2", overwrite = TRUE)
saveRDS(jumping_jack_maxed_out, file = "inst/extdata/jumping_jack_maxed_out.rds", compress = TRUE)