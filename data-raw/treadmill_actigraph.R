require(stringr)
require(plyr)
require(dplyr)
require(doSNOW)
require(mHealthR)
require(Counts)

cl = makeCluster(4, type = "SOCK")
registerDoSNOW(cl)

folder = "../../data/Treadmill/";
files = list.files(path = folder, pattern = ".*RAW5sec\\.csv", full.names = TRUE, recursive = FALSE)

# read in session file
sessions = read.csv(file = file.path(folder, "sessions.csv"), stringsAsFactors = FALSE)
sessions["START_TIME"] = paste("2017-03-22",sessions[["START_TIME"]])
sessions["STOP_TIME"] = paste("2017-03-22",sessions[["STOP_TIME"]])

# read in raw and divide chunks and make extra versions for actigraph

csvData_actigraph = ldply(files, function(sensor_file){
  if(stringr::str_detect(sensor_file, "Waist")){
    sensorLocation = "Waist"
  }else if(stringr::str_detect(sensor_file, "Wrist")){
    sensorLocation = "Nondominant wrist"
  }
  sr = 80
  if(stringr::str_detect(sensor_file, "CLE")){
    id = "GT3X+"
    gr = 6
  }else if(stringr::str_detect(sensor_file, "TAS")){
    id = "GT9X"
    gr = 8
  }
  actigraph_count_data = import_actigraph_count(sensor_file)

  sessions = sessions %>% dplyr::filter(PID %in% 3:6)

  segmentedData = adply(sessions, 1, function(seg){
    segData = mHealthR::mhealth.clip(actigraph_count_data, start_time = seg$START_TIME, stop_time = seg$STOP_TIME, file_type = "sensor")
    segData$ID = id
    segData$SR = sr
    segData$GRANGE = gr
    segData$LOCATION = sensorLocation
    return(segData)
  }, .progress = progress_text(), .parallel = TRUE)
  return(segmentedData)
}, .parallel = FALSE, .progress = "text", .id = NULL, .inform = TRUE)

csvData_actigraph = csvData_actigraph[c(5:6, 1:4, 7:10)]

stopCluster(cl)
saveRDS(csvData_actigraph, "inst/extdata/treadmill_actigraph.rds", compress = TRUE)
