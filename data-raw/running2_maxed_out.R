require(mHealthR)
startTime = "2015-12-15 13:51:47.785"
endTime = "2015-12-15 13:52:02.085"

runningData = mhealth.read("../../data/counts/running2_maxed_out/ActigraphGT9X-AccelerationCalibrated-NA.TAS1E23150167-AccelerationCalibrated.2015-12-15-13-00-00-000-M0500.sensor.csv.gz", filetype = "sensor")

runningData_clip = mhealth.clip(runningData, start_time = startTime, stop_time = endTime, file_type = "sensor")

runningData_crop = runningData_clip[,2:4] %>% adply(.margins = 2, function(colData){
  selection = sum(colData > 2)
  colData[colData > 2] = 2 + rnorm(selection, 0, 0.03)
  selection = sum(colData < -2)
  colData[colData < -2] = -2 + rnorm(selection, 0, 0.03)
  return(colData[[1]])
}, .id = NULL)

runningData_crop = data.frame(t(runningData_crop), row.names = NULL)
runningData_crop = cbind(runningData_clip[[1]], runningData_crop)
names(runningData_crop) = names(runningData)

running2_maxed_out = rbind(cbind(runningData_clip, GRANGE = 8), cbind(runningData_crop, GRANGE = 2))
running2_maxed_out$SR = 80
running2_maxed_out$TYPE = "GT9X"
running2_maxed_out$LOCATION = "NondominantWrist"

# devtools::use_data(running2_maxed_out, compress = "bzip2", overwrite = TRUE)
saveRDS(running2_maxed_out, file = "inst/extdata/running2_maxed_out.rds", compress = TRUE)