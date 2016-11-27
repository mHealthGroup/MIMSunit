# create exported dataset for mhealth package
# 
rm(list = ls(all.names = TRUE))
require(stringr)
require(plyr)
require(dplyr)

folder = "offline_data/shaker1/";

# shaker raw data -----------
files = list.files(path = folder, all.files = FALSE, full.names = TRUE, pattern = ".*.csv", recursive = TRUE)
files = normalizePath(unlist(files))

shaker1 = ldply(files, function(file){
  fileParts = file %>% SensorData.getFilenameParts
  sr = fileParts$sensorType
  shakerFreq = fileParts$dataType %>% str_match(pattern = "Shaker([0-9]+)Hz") %>% nth(2) %>% as.numeric
  id = fileParts$sensorId %>% str_match(pattern = "(.+)RAW") %>% nth(2)
  data = SensorData.importCsv(file)
  data = cbind(data, LOCATION = "Shaker", SR = sr, RPM = shakerFreq, DEVICE = id, GRANGE = "8", stringsAsFactors = FALSE)
  return(data)
}, .parallel = FALSE, .progress = "text", .id = NULL, .inform = TRUE)

# save as exported data
# devtools::use_data(shaker1, compress = "bzip2", overwrite = TRUE)
saveRDS(shaker1, file = "inst/extdata/shaker1.rds", compress = TRUE)
