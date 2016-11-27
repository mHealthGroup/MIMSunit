# create exported dataset for mhealth package
# 
rm(list = ls(all.names = TRUE))
require(stringr)
require(plyr)
require(dplyr)

folder = "offline_data/shaker4/";

# shaker raw data -----------
files = list.files(path = folder, all.files = FALSE, full.names = TRUE, pattern = ".*.csv", recursive = FALSE)
files = files[!str_detect(files, "sessions")]
files = files[!str_detect(files, "actigraph")]
files = normalizePath(unlist(files))

shaker4 = ldply(files, function(file){
  data = SensorData.importCsv(file, violate = TRUE)
  if(str_detect(file, "phone")){
    sr = 50
    id = "Nexus 4"
    gr = "4"
  }else if(str_detect(file, "watch")){
    data = SensorData.offset(data, offsetValue = -60 * 22 + 20)
    sr = 100
    id = "LG Watch R Urbane"
    gr = "2"
  }
  
  # cut into segments
  sessions = read.csv(file.path(folder, "sessions.csv"), header = TRUE, stringsAsFactors = FALSE)
  segmented = sessions %>% adply(1, function(row){
    segment = data %>% SensorData.clip(row[,1], row[,2])
    rpm = row[,3]
    hz = row[,4]
    return(segment %>% cbind(LOCATION = "Shaker", SR = sr, RPM = hz, DEVICE = id, GRANGE = gr, stringsAsFactors = FALSE))
  }, .inform = TRUE, .id = NULL) %>% 
    subset(select = -(1:3))
  return(segmented)
}, .parallel = FALSE, .progress = "text", .id = NULL, .inform = TRUE)

names(shaker4)[2:4] = c("X_ACCELATION_METERS_PER_SECOND_SQUARED","Y_ACCELATION_METERS_PER_SECOND_SQUARED", "Z_ACCELATION_METERS_PER_SECOND_SQUARED");

# save as exported data
# devtools::use_data(shaker4, compress = "bzip2", overwrite = TRUE)
saveRDS(shaker4, file = "inst/extdata/shaker4.rds", compress = TRUE)

# generate Actigraph version
l_ply(files, function(file){
  data = SensorData.importCsv(file, violate = TRUE)
  if(str_detect(file, "phone")){
    sr = 50
    id = "Nexus 4"
    gr = "4"
    filename = "phone.actigraph.csv"
    
  }else if(str_detect(file, "watch")){
    data = SensorData.offset(data, offsetValue = -60 * 22 + 20)
    sr = 100
    id = "LG Watch R Urbane"
    gr = "2"
    filename = "watch.actigraph.csv"
  }
  header = SensorData.createActigraphCsvHeader(startTime = data[1,1], 
                                               downloadTime = tail(data, 1)[1], 
                                               samplingRate = sr, 
                                               sensorId = id, 
                                               firmVersion = "2.2.0", 
                                               softVersion = "6.13.2")
  SensorData.io.writeAsActigraphRaw(folder = folder, data, header, filename)
}, .parallel = FALSE, .progress = "text", .inform = TRUE)
