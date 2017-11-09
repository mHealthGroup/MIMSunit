# create exported dataset for mhealth package
#
rm(list = ls(all.names = TRUE))
require(stringr)
require(plyr)
require(dplyr)
require(SMARTcounts)
require(lubridate)
require(mHealthR)
folder = "F:/data/shaker5/";

# shaker raw data -----------
raw_files = list.files(path = file.path(folder, 'enmo'), all.files = FALSE, full.names = TRUE, pattern = ".*Epoch.*.csv", recursive = FALSE)
raw_files = raw_files[!str_detect(raw_files, "sessions")]
raw_files = normalizePath(unlist(raw_files))

shaker5_biobank = ldply(raw_files, function(file){
  tokens = stringr::str_split(basename(file), "_")
  sr = as.numeric(str_split(tokens[[1]][2], "Hz")[[1]][1])
  id = tokens[[1]][1]
  gr = as.numeric(str_split(tokens[[1]][3], "gEpoch")[[1]][1])
  name = paste0("ENMO_", str_split(tokens[[1]][4], "\\.")[[1]][1])
  enmo_data = SMARTcounts::import_biobank_enmo(file)
  enmo_data[1] = force_tz(enmo_data[1], tzone = Sys.timezone())
  enmo_data = enmo_data[c(1,2)]
  enmo_data$name = name
  # cut into segments
  sessions = read.csv(file.path(folder, "sessions.csv"), header = TRUE, stringsAsFactors = FALSE)
  segmented = sessions %>% adply(1, function(row){
    segment = enmo_data %>% mHealthR::mhealth.clip(start_time = row[,1], stop_time = row[,2], file_type = "sensor")
    rpm = row[,3]
    hz = row[,4]
    return(segment %>% cbind(LOCATION = "Shaker", SR = sr, RPM = hz, DEVICE = id, GRANGE = gr, stringsAsFactors = FALSE))
  }, .inform = TRUE, .id = NULL) %>%
    subset(select = -(1:3))
  return(segmented)
}, .parallel = FALSE, .progress = "text", .id = NULL, .inform = TRUE)

# save as exported data
saveRDS(shaker5_biobank, file = "inst/extdata/shaker5_biobank_enmo.rds", compress = TRUE)
