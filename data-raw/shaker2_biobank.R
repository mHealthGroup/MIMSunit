# create exported dataset for mhealth package
#
rm(list = ls(all.names = TRUE))
require(stringr)
require(plyr)
require(dplyr)
require(MIMSunit)
require(lubridate)
require(mHealthR)
folder = "F:/data/shaker2/";

# shaker raw data -----------
raw_files = list.files(path = folder, all.files = FALSE, full.names = TRUE, pattern = ".*Epoch.csv", recursive = FALSE)
raw_files = raw_files[!str_detect(raw_files, "sessions")]
raw_files = normalizePath(unlist(raw_files))

shaker2_biobank = ldply(raw_files, function(file){
  tokens = stringr::str_split(basename(file), "_")
  sr = as.numeric(str_split(tokens[[1]][2], "Hz")[[1]][1])
  id = tokens[[1]][1]
  gr = as.numeric(str_split(tokens[[1]][3], "gEpoch")[[1]][1])
  data = import_biobank_enmo(file)
  data[1] = force_tz(data[1], tzone = Sys.timezone())
  data = data[c(1,2)]
  # cut into segments
  sessions = read.csv(file.path(folder, "sessions.csv"), header = TRUE, stringsAsFactors = FALSE)
  segmented = sessions %>% adply(1, function(row){
    segment = data %>% mHealthR::mhealth.clip(start_time = row[,1], stop_time = row[,2], file_type = "sensor")
    rpm = row[,3]
    hz = row[,4]
    return(segment %>% cbind(LOCATION = "Shaker", SR = sr, RPM = hz, DEVICE = id, GRANGE = gr, stringsAsFactors = FALSE))
  }, .inform = TRUE, .id = NULL) %>%
    subset(select = -(1:3))
  return(segmented)
}, .parallel = FALSE, .progress = "text", .id = NULL, .inform = TRUE)

# save as exported data
# devtools::use_data(shaker2_actigraph, compress = "bzip2", overwrite = TRUE)
saveRDS(shaker2_biobank, file = "inst/extdata/shaker2_biobank_enmo.rds", compress = TRUE)
