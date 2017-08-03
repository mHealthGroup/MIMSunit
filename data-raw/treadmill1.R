require(stringr)
require(plyr)
require(dplyr)
require(doSNOW)
require(mHealthR)

folder = "offline_data/walkrun1/";
subjects = list.dirs(path = folder, full.names = FALSE, recursive = FALSE)
subjects = subjects[str_detect(subjects, "P")]
ready_folder = "ready"
agd_folder = "agd"
raw_folder = "raw"
actigraph_folder = "actigraph"
segment_file = "segments.csv"

makeNewSensor = function(oldData, new_range, new_sr){
  newData = SensorData.interpolate(oldData, method = "spline_natural", sr = new_sr)
  newData = SensorData.crop(newData, range = new_range)
  return(newData)
}

# read in raw and divide chunks and make extra versions for actigraph
files = llply(subjects, function(subj){
  raw_path = file.path(folder, subj, raw_folder);
  rawFiles = list.files(path = raw_path, all.files = FALSE, full.names = TRUE, pattern = ".*.csv")
  rawFiles = rawFiles[!str_detect(rawFiles, "segment")]
  return(rawFiles)
}, .progress = "text") %>% unlist %>% normalizePath

csvData_raw = ldply(files, function(file){
  segments = read.csv(file = file.path(dirname(file), segment_file), stringsAsFactors = FALSE)
  tokens = str_split(basename(file), pattern = "_")[[1]]
  sensorLocation = tokens[1]
  subj = basename(dirname(dirname((file))))
  subj_tokens = str_split(subj, pattern = "-")
  subj_name = subj_tokens[[1]][1]
  session_name = subj_tokens[[1]][2]
  sr = tokens[2]
  gr = str_split(tokens[3], pattern = "\\.")[[1]][1]
  id = "GT9X"
  csvData = import_actigraph_raw(file)

  # make extra versions for actigraph
  if(str_detect(file, "_100_")){
    actigraphHeader = import_actigraph_meta(file)
    actigraph_path = file.path(dirname(dirname(file)), actigraph_folder)

    dir.create(actigraph_path)

    # ActivPal3
    sr = 20
    newData = makeNewSensor(csvData, new_range = c(-2,2), new_sr = sr)
    headerStr = SensorData.createActigraphCsvHeader(startTime = actigraphHeader$st, downloadTime = actigraphHeader$dt, samplingRate = sr, sensorId = actigraphHeader$sn, firmVersion = actigraphHeader$fw, softVersion = actigraphHeader$sw)
    SensorData.io.writeAsActigraphRaw(folder = actigraph_path, sensorData = newData, headerStr = headerStr, custom_name = paste0("ActivPal3_", basename(file)))

    # GT3XBT
    sr = 40
    newData = makeNewSensor(csvData, new_range = c(-6,6), new_sr = sr)
    headerStr = SensorData.createActigraphCsvHeader(startTime = actigraphHeader$st, downloadTime = actigraphHeader$dt, samplingRate = sr, sensorId = stringr::str_replace(actigraphHeader$sn, "TAS", "CLE"), firmVersion = actigraphHeader$fw, softVersion = actigraphHeader$sw)
    SensorData.io.writeAsActigraphRaw(folder = actigraph_path, sensorData = newData, headerStr = headerStr, custom_name = paste0( "GT3XBT_40_", basename(file)))

    # GT3XBT
    sr = 80
    newData = makeNewSensor(csvData, new_range = c(-6,6), new_sr = sr)
    headerStr = SensorData.createActigraphCsvHeader(startTime = actigraphHeader$st, downloadTime = actigraphHeader$dt, samplingRate = sr, sensorId = stringr::str_replace(actigraphHeader$sn, "TAS", "CLE"), firmVersion = actigraphHeader$fw, softVersion = actigraphHeader$sw)
    SensorData.io.writeAsActigraphRaw(folder = actigraph_path, sensorData = newData, headerStr = headerStr, custom_name = paste0("GT3XBT_80_", basename(file)))

    sr = 100
    newData = makeNewSensor(csvData, new_range = c(-2,2), new_sr = sr)
    headerStr = SensorData.createActigraphCsvHeader(startTime = actigraphHeader$st, downloadTime = actigraphHeader$dt, samplingRate = sr, sensorId = actigraphHeader$sn, firmVersion = actigraphHeader$fw, softVersion = actigraphHeader$sw)
    SensorData.io.writeAsActigraphRaw(folder = actigraph_path, sensorData = newData, headerStr = headerStr, custom_name = paste0("LG Urbane R_", basename(file)))

    sr = 30
    newData = makeNewSensor(csvData, new_range = c(-3,3), new_sr = sr)
    headerStr = SensorData.createActigraphCsvHeader(startTime = actigraphHeader$st, downloadTime = actigraphHeader$dt, samplingRate = sr, sensorId = stringr::str_replace(actigraphHeader$sn, "TAS", "MAT"), firmVersion = actigraphHeader$fw, softVersion = actigraphHeader$sw)
    SensorData.io.writeAsActigraphRaw(folder = actigraph_path, sensorData = newData, headerStr = headerStr, custom_name = paste0("GT3X_", basename(file)))

    sr = 50
    newData = makeNewSensor(csvData, new_range = c(-4,4), new_sr = sr)
    headerStr = SensorData.createActigraphCsvHeader(startTime = actigraphHeader$st, downloadTime = actigraphHeader$dt, samplingRate = sr, sensorId = actigraphHeader$sn, firmVersion = actigraphHeader$fw, softVersion = actigraphHeader$sw)
    SensorData.io.writeAsActigraphRaw(folder = actigraph_path, sensorData = newData, headerStr = headerStr, custom_name = paste0("Nexus 4_", basename(file)))

    sr = 60
    newData = makeNewSensor(csvData, new_range = c(-8,8), new_sr = sr)
    headerStr = SensorData.createActigraphCsvHeader(startTime = actigraphHeader$st, downloadTime = actigraphHeader$dt, samplingRate = sr, sensorId = actigraphHeader$sn, firmVersion = actigraphHeader$fw, softVersion = actigraphHeader$sw)
    SensorData.io.writeAsActigraphRaw(folder = actigraph_path, sensorData = newData, headerStr = headerStr, custom_name = paste0( "GT9X_60_8_", basename(file)))

    sr = 100
    headerStr = SensorData.createActigraphCsvHeader(startTime = actigraphHeader$st, downloadTime = actigraphHeader$dt, samplingRate = sr, sensorId = actigraphHeader$sn, firmVersion = actigraphHeader$fw, softVersion = actigraphHeader$sw)
    SensorData.io.writeAsActigraphRaw(folder = actigraph_path, sensorData = csvData, headerStr = headerStr, custom_name = paste0("GT9X_100_8_", basename(file)))
  }


  segmentedData = adply(segments, 1, function(seg){
    require(mhealthformatsupportr)
    segData = SensorData.clip(csvData, startTime = seg$START_TIME, endTime = seg$END_TIME)
    return(segData)
  }, .progress = progress_text(), .parallel = TRUE)

  segmentedData$ID = "GT9X"
  segmentedData$SR = sr
  segmentedData$GRANGE = gr
  segmentedData$LOCATION = sensorLocation
  segmentedData$SUBJECT = subj_name
  segmentedData$SESSION = session_name
  return(segmentedData)
}, .parallel = FALSE, .progress = "text", .id = NULL, .inform = TRUE)

csvData_raw = csvData_raw[c(1, 2, 5,6,7,8, 3,4,9:14)]
csvData_raw = csvData_raw[c(-1,-2)]
stopCluster(cl)
saveRDS(csvData_raw, "inst/extdata/treadmill1.rds", compress = TRUE)
