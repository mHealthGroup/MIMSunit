require(stringr)
require(plyr)
require(dplyr)
require(lubridate)
folder = "offline_data/walkrun1/";
subjects = list.dirs(path = folder, full.names = FALSE, recursive = FALSE)
subjects = subjects[str_detect(subjects, "P")]
ready_folder = "ready"
raw_folder = "raw"
actigraph_folder = "actigraph"
segment_file = "segments.csv"

files = llply(subjects, function(subj){
  raw_path = file.path(folder, subj, actigraph_folder);
  rawFiles = list.files(path = raw_path, all.files = FALSE, full.names = TRUE, pattern = ".*.csv")
  rawFiles = rawFiles[!str_detect(rawFiles, "ActivPal")]
  return(rawFiles)
}, .progress = "text") %>% unlist %>% normalizePath

treadmill1_count_actigraph = ldply(files, function(file){
  header = file %>% import_actigraph_meta()
  sr = header$sr
  tokens = str_split(basename(file), "_")[[1]]
  id = tokens[1]
  gr = header$gr
  if(str_detect(id, "Nexus")) gr = 4
  if(str_detect(id, "LG")) gr = 2
  location = tokens[2]
  count_filename = paste0(str_split(basename(file), "\\.")[[1]][1], "5sec.csv")
  data = import_actigraph_count_csv(file.path(dirname(file), "counts", count_filename), count_col = 2, count_per_axis_cols = NULL)
  data[1] = force_tz(data[1], tzone = Sys.timezone())
  # cut into segments
  sessions = read.csv(file.path(dirname(dirname(file)), raw_folder, "segments.csv"), header = TRUE, stringsAsFactors = FALSE)
  segmented = sessions %>% adply(1, function(row){
    segment = data %>% mhealth.clip(row[,1], row[,2],file_type = "sensor")
    mph = row[,3]
    weight = row[,4]
    return(segment %>% cbind(LOCATION = location, SR = sr, MPH = mph, ID = id, GRANGE = gr, WEGITHS = weight, stringsAsFactors = FALSE))
  }, .inform = TRUE, .id = NULL) %>%
    subset(select = -(1:3))
  return(segmented)
}, .parallel = FALSE, .progress = "text", .id = NULL, .inform = TRUE)

saveRDS(treadmill1_count_actigraph, "inst/extdata/treadmill1_count_actigraph.rds", compress = TRUE)