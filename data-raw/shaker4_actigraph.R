# create exported dataset for mhealth package
# 
rm(list = ls(all.names = TRUE))
require(stringr)
require(plyr)
require(dplyr)
require(lubridate)
folder = "offline_data/shaker4/";

# shaker raw data -----------
raw_files = list.files(path = folder, all.files = FALSE, full.names = TRUE, pattern = ".*.csv", recursive = FALSE)
raw_files = raw_files[str_detect(raw_files, "actigraph")]
raw_files = normalizePath(unlist(raw_files))

shaker4_actigraph = ldply(raw_files, function(file){
  if(str_detect(file, "phone")){
    sr = 50
    id = "Nexus 4"
    gr = 4
  }else if(str_detect(file, "watch")){
    sr = 100
    id = "LG Watch R Urbane"
    gr = 2
  }
  data = SensorData.importActigraphCountCsv(file.path(folder, "agd", basename(file)), axes = c(2,3), count_col_name = "ACTIGRAPH")
  data[1] = force_tz(data[1], tzone = Sys.timezone())
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

# save as exported data
# devtools::use_data(shaker4_actigraph, compress = "bzip2", overwrite = TRUE)
saveRDS(shaker4_actigraph, file = "inst/extdata/shaker4_count_actigraph.rds", compress = TRUE)