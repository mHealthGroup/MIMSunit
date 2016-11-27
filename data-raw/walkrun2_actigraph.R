# create exported dataset for mhealth package
# 
rm(list = ls(all.names = TRUE))
require(stringr)
require(plyr)
require(dplyr)
require(lubridate)
require(mhealthformatsupportr)
folder = "offline_data/walkrun2/";

# shaker raw data -----------
raw_files = list.files(path = folder, all.files = FALSE, full.names = TRUE, pattern = ".*.csv", recursive = TRUE)
raw_files = normalizePath(unlist(raw_files))
raw_files = raw_files[str_detect(basename(dirname(raw_files)), "-") & !str_detect(basename(dirname(raw_files)), "P4|P5") & !str_detect(raw_files, "segments")]


walkrun2_actigraph = ldply(raw_files, function(file){
  header = file %>% SensorData.parseActigraphCsvHeader(header = TRUE)
  sr = header$sr
  id = "GT3X"
  gr = header$gr
  location = "DominantWaist"
  data = SensorData.importActigraphCountCsv(file.path(dirname(file), "agd", basename(file)), count_col = 2, count_col_name = "ACTIGRAPH")
  data[1] = force_tz(data[1], tzone = Sys.timezone())
  # cut into segments
  sessions = read.csv(file.path(dirname(file), "segments.csv"), header = TRUE, stringsAsFactors = FALSE)
  segmented = sessions %>% adply(1, function(row){
    segment = data %>% SensorData.clip(row[,1], row[,2])
    mph = row[,3]
    weight = 0
    return(segment %>% cbind(LOCATION = location, SR = sr, MPH = mph, DEVICE = id, GRANGE = gr, stringsAsFactors = FALSE))
  }, .inform = TRUE, .id = NULL) %>% 
    subset(select = -(1:3))
  return(segmented)
}, .parallel = FALSE, .progress = "text", .id = NULL, .inform = TRUE)

# save as exported data
devtools::use_data(walkrun2_actigraph, compress = "bzip2", overwrite = TRUE)
