# create exported dataset for mhealth package
# 
rm(list = ls(all.names = TRUE))
require(stringr)
require(plyr)
require(dplyr)
require(lubridate)
folder = "offline_data/shaker2/";

# shaker raw data -----------
raw_files = list.files(path = folder, all.files = FALSE, full.names = TRUE, pattern = ".*.csv", recursive = FALSE)
raw_files = raw_files[!str_detect(raw_files, "sessions")]
raw_files = normalizePath(unlist(raw_files))

shaker2_actigraph = ldply(raw_files, function(file){
  header = file %>% SensorData.parseActigraphCsvHeader(header = TRUE)
  sr = header$sr
  id = header$sn
  gr = header$gr
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
# devtools::use_data(shaker2_actigraph, compress = "bzip2", overwrite = TRUE)
saveRDS(shaker2_actigraph, file = "inst/extdata/shaker2_count_actigraph.rds", compress = TRUE)
