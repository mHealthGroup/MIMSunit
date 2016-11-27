# create exported dataset for mhealth package
# 
rm(list = ls(all.names = TRUE))
require(stringr)
require(plyr)
require(dplyr)

folder = "offline_data/shaker3/";

# shaker raw data -----------
files = list.files(path = folder, all.files = FALSE, full.names = TRUE, pattern = ".*.csv", recursive = FALSE)
files = files[!str_detect(files, "sessions")]
files = normalizePath(unlist(files))

shaker3 = ldply(files, function(file){
  if(str_detect(file, "ActivPal")){
    data = SensorData.importCsv(file, violate = TRUE)
    sr = SensorData.getSamplingRate(data)
    id = "ActivPal3"
    gr = 2
  }else{
    header = file %>% SensorData.parseActigraphCsvHeader(header = TRUE)
    sr = header$sr
    id = header$sn
    gr = header$gr
    data = SensorData.importActigraphCsv(file)
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

# save as external data
# devtools::use_data(shaker3, compress = "bzip2", overwrite = TRUE)
saveRDS(shaker3, file = "inst/extdata/shaker3.rds", compress = TRUE)
