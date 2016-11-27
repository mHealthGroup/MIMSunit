startTime = as.POSIXct("2015-12-15 13:51:47.785")
endTime = as.POSIXct("2015-12-15 13:52:02.085")

runningData = SensorData.importCsv("offline_data/running2_maxed_out/ActigraphGT9X-AccelerationCalibrated-NA.TAS1E23150167-AccelerationCalibrated.2015-12-15-13-00-00-000-M0500.sensor.csv.gz")

runningData_clip = SensorData.clip(runningData, startTime = startTime, endTime = endTime)

runningData_crop = runningData_clip[,2:4] %>% adply(.margins = 2, function(colData){
  selection = sum(colData > 3)
  colData[colData > 3] = 3 + rnorm(selection, 0, 0.05)
  selection = sum(colData < -3)
  colData[colData < -3] = -3 + rnorm(selection, 0, 0.05)
  return(colData[[1]])
}, .id = NULL)

runningData_crop = data.frame(t(runningData_crop), row.names = NULL)
runningData_crop = cbind(runningData_clip[[1]], runningData_crop)
names(runningData_crop) = names(runningData)

running2_maxed_out = rbind(cbind(runningData_clip, GRANGE = 8), cbind(runningData_crop, GRANGE = 3))
running2_maxed_out$SR = 80
running2_maxed_out$TYPE = "GT9X"
running2_maxed_out$LOCATION = "NondominantWrist"

# devtools::use_data(running2_maxed_out, compress = "bzip2", overwrite = TRUE)
saveRDS(running2_maxed_out, file = "inst/extdata/running2_maxed_out.rds", compress = TRUE)