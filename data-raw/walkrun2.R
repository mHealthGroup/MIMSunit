# create exported dataset for mhealth package
# 
rm(list = ls(all.names = TRUE))
require(stringr)
require(plyr)
require(dplyr)

folder = "offline_data/walkrun2/";
subjects = list.dirs(path = folder, full.names = FALSE, recursive = FALSE)
subjects = subjects[str_detect(subjects, "P")]

# walk run raw data -----------


rawFiles = folder %>% list.files(all.files = FALSE, full.names = TRUE, pattern = ".*.csv", recursive = TRUE)
files = rawFiles %>% unlist %>% normalizePath

files = files[!str_detect(files, "segments|agd") & !str_detect(files, "P4") & !str_detect(files, "P5")]

walkrun2 = files %>% ldply(function(file){
  tokens <- file %>% dirname %>% basename %>% str_split(pattern = "-") %>% unlist
  subj_name <- tokens[1]
  session_name <- tokens[2]
  sensorLocation = "DominantWaist"
  
  segments = file %>% dirname %>% file.path("segments.csv") %>% read.csv(stringsAsFactors = FALSE, header = TRUE)
  header = file %>% SensorData.parseActigraphCsvHeader(header = FALSE)

  data = file %>% 
    SensorData.importActigraphCsv(ad_convert = FALSE, ts_provided = FALSE, header_provided = FALSE)
  
  data[-1] = data[-1] / 3200 * 6 - 3
  
  data = data %>%
    cbind(LOCATION = sensorLocation, SR = as.character(header$sr), DEVICE = "GT3X", GRANGE = 3, MPH = NA, WEIGHT = "0", SUBJECT = subj_name, SESSION = session_name)
  
  processed = segments %>% adply(1, function(row){
    clipped = data %>% SensorData.clip(row$START_TIME, row$END_TIME)
    clipped$MPH = row$MPH
    return(clipped)
  }, .progress = "text", .id = NULL, .inform = TRUE)
  
  return(processed)
}, .parallel = FALSE, .progress = "text", .id = NULL, .inform = TRUE)

walkrun2 = walkrun2[,-c(1,2)]

# save as exported data
devtools::use_data(walkrun2, compress = "bzip2", overwrite = TRUE)