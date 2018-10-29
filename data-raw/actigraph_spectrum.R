
require(plyr)
require(dplyr)
require(doSNOW)

cl = makeCluster(4, type = "SOCK")
registerDoSNOW(cl)


l_ply(seq(30, 100, by = 10), function(sr){

  fs = c(seq(0, 1, by = 0.05), seq(1, 50, by = 0.5))
  fs = fs[-1]
  #Create test files as raw actigraph csv
  l_ply(fs, function(f){
    require(mhealthformatsupportr)
    require(stringr)
    x = seq(as.POSIXct("2010-01-01 18:00:00"), as.POSIXct("2010-01-01 18:02:00"), by = 1/sr)
    y = sin(2* pi * f * as.numeric(x - x[1]) + runif(n = 1, min = -pi, max = pi))
    path = file.path("offline_data/actigraph_spectrum/")
    data = data.frame(x, y, y, y)
    headStr = SensorData.createActigraphCsvHeader(as.POSIXct("2010/01/01 18:00:00"), as.POSIXct("2010-01-01 18:02:00"), samplingRate = sr, sensorId = "TASACTIGRAPHSPECTRUM", firmVersion = "1.4.0", softVersion = "6.13.2")
    SensorData.io.writeAsActigraphRaw(path, data, headStr, custom_name = paste0("sr", sr, "_f",str_replace(f, "\\.", "o"), ".csv"))
  }, .progress = progress_text(), .parallel = FALSE)
}, .progress = progress_text(), .parallel = FALSE)

stopCluster(cl)


# load back actigraph count from the created test files
actigraph_response = ldply(seq(30, 100, by = 10), function(sr){
  fs = c(seq(0, 1, by = 0.05), seq(1, 50, by = 0.5))
  fs = fs[-1]
  actigraph_spectrum = ldply(fs, function(f){
    data = import_actigraph_count_csv(paste0("offline_data/actigraph_spectrum/actigraph_counts/sr",sr, "_f", str_replace(f, "\\.", "o"),"30sec.csv"), count_col = 2, count_per_axis_cols = NULL)
    return(data.frame("FREQ" = f, "VALUE" = mean(data[,2])))
  }, .progress = progress_text())
  actigraph_spectrum["SR"] = sr
  return(actigraph_spectrum)
})

actigraph_response = actigraph_response[c(3,1,2)]
actigraph_response[,"VALUE"] = actigraph_response[,"VALUE"] / max(actigraph_response[,"VALUE"])

# devtools::use_data(actigraph_spectrum, compress = "bzip2", overwrite = TRUE)
saveRDS(actigraph_response, "inst/extdata/actigraph_response.rds", compress = TRUE)
