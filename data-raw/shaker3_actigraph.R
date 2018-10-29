# create exported dataset for mhealth package
#
rm(list = ls(all.names = TRUE))
require(stringr)
require(plyr)
require(dplyr)
require(lubridate)
require(mHealthR)
require(SMARTcounts)
folder = "F:/data/shaker3/";

# shaker raw data -----------
raw_files = list.files(path = folder, all.files = FALSE, full.names = TRUE, pattern = ".*.csv", recursive = FALSE)
raw_files = raw_files[!str_detect(raw_files, "sessions")]
raw_files = normalizePath(unlist(raw_files))

shaker3_actigraph = ldply(raw_files, function(file){
  if(str_detect(file, "ActivPal")){
    sr = 20
    id = "ActivPal3"
    gr = 2
  }else{
    header = file %>% import_actigraph_meta(header = TRUE)
    sr = header$sr
    id = header$sn
    gr = header$gr
  }
  data = import_actigraph_count_csv(file.path(folder, "agd", basename(file)), count_col = NULL, count_per_axis_cols = c(2,3, 4))
  data = data[,1:2]
  data[1] = force_tz(data[1], tzone = Sys.timezone())
  # cut into segments
  sessions = read.csv(file.path(folder, "sessions.csv"), header = TRUE, stringsAsFactors = FALSE)
  segmented = sessions %>% adply(1, function(row){
    segment = data %>% mhealth.clip(row[,1], row[,2], file_type = "sensor")
    rpm = row[,3]
    hz = row[,4]
    return(segment %>% cbind(LOCATION = "Shaker", SR = sr, RPM = hz, DEVICE = id, GRANGE = gr, stringsAsFactors = FALSE))
  }, .inform = TRUE, .id = NULL) %>%
    subset(select = -(1:3))
  return(segmented)
}, .parallel = FALSE, .progress = "text", .id = NULL, .inform = TRUE)

# save as exported data
# devtools::use_data(shaker3_actigraph, compress = "bzip2", overwrite = TRUE)
saveRDS(shaker3_actigraph, file = "inst/extdata/shaker3_count_actigraph.rds", compress = TRUE)
