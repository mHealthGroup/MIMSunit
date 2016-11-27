startTime = as.POSIXct("2015-10-08 15:02:21.094")
endTime = as.POSIXct("2015-10-08 15:06:38.580")

 frisbeeData = SensorData.importCsv("offline_data/frisbee_maxed_out/ActigraphGT9X-AccelerationCalibrated-NA.TAS1E23150141-AccelerationCalibrated.2015-10-08-15-00-00-000-M0400.sensor.csv.gz")

frisbeeData_clip = SensorData.clip(frisbeeData, startTime = startTime, endTime = endTime)

frisbeeData_crop = frisbeeData_clip[,2:4] %>% adply(.margins = 2, function(colData){
  selection = sum(colData > 3)
  colData[colData > 3] = 3 + rnorm(selection, 0, 0.05)
  selection = sum(colData < -3)
  colData[colData < -3] = -3 + rnorm(selection, 0, 0.05)
  return(colData[[1]])
}, .id = NULL)

frisbeeData_crop = data.frame(t(frisbeeData_crop), row.names = NULL)
frisbeeData_crop = cbind(frisbeeData_clip[[1]], frisbeeData_crop)
names(frisbeeData_crop) = names(frisbeeData)

frisbee_maxed_out = rbind(cbind(frisbeeData_clip, GRANGE = 8), cbind(frisbeeData_crop, GRANGE = 3))
frisbee_maxed_out$SR = 80
frisbee_maxed_out$TYPE = "GT9X"
frisbee_maxed_out$LOCATION = "DominantWrist"

# devtools::use_data(frisbee_maxed_out, compress = "bzip2", overwrite = TRUE)
saveRDS(frisbee_maxed_out, file = "inst/extdata/frisbee_maxed_out.rds", compress = TRUE)