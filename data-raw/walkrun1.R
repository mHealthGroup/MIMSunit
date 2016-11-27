# create exported dataset for mhealth package
# 
rm(list = ls(all.names = TRUE))
require(stringr)
require(plyr)

folder = "offline_data/walkrun1/";
subjects = list.dirs(path = folder, full.names = FALSE, recursive = FALSE)
subjects = subjects[str_detect(subjects, "P")]
ready_folder = "ready"
actigraphcsv_folder = "actigraphcsv"

# walk run raw data -----------

files = llply(subjects, function(subj){
  ready_path = file.path(folder, subj, ready_folder);
  rawFiles = list.files(path = ready_path, all.files = FALSE, full.names = TRUE, pattern = ".*.csv")
  return(rawFiles)
}, .progress = "text")

files = normalizePath(unlist(files))

walkrun1 = ldply(files, function(file){
  tokens = str_split(basename(file), pattern = "_")[[1]]
  sensorLocation = tokens[1]
  sr = tokens[2]
  gr = 8
  id = "GT9X"
  speed = as.numeric(str_sub(tokens[3], start = 4))
  weight = str_split(tokens[4], "\\.")[[1]][1]
  subj = basename(dirname(dirname(file)))
  subj_tokens = str_split(subj, pattern = "-")
  subj_name = subj_tokens[[1]][1]
  session_name = subj_tokens[[1]][2]
  data = SensorData.importCsv(file, violate = TRUE)
  data = cbind(data, LOCATION = sensorLocation, SR = sr, GRANGE = gr, DEVICE = "GT9X", MPH = speed, WEIGHT = weight, SUBJECT = subj_name, SESSION = session_name, stringsAsFactors = FALSE)
  
  return(data)
}, .parallel = FALSE, .progress = "text", .id = NULL, .inform = TRUE)

# save as exported data
devtools::use_data(walkrun1, compress = "bzip2", overwrite = TRUE)

# walk run actigraph count data ---------
files = llply(subjects, function(subj){
  ready_path = file.path(folder, subj, actigraphcsv_folder, "agd");
  rawFiles = list.files(path = ready_path, all.files = FALSE, full.names = TRUE, pattern = ".*.csv", recursive = TRUE)
  return(rawFiles)
}, .progress = "text")

files = normalizePath(unlist(files))

walkrun1_actigraph = ldply(files, function(file){
  tokens = str_split(basename(file), pattern = "_")[[1]]
  sensorLocation = tokens[1]
  sr = tokens[2]
  gr = 8
  speed = as.numeric(str_sub(tokens[3], start = 4))
  weight = tokens[4]
  epoch = str_match(str_split(tokens[5], "\\.")[[1]][1], "([0-9]+)sec")[2] %>% paste("sec")
  subj = basename(dirname(dirname(dirname(dirname((file))))))
  subj_tokens = str_split(subj, pattern = "-")
  subj_name = subj_tokens[[1]][1]
  session_name = subj_tokens[[1]][2]
  data = SensorData.importActigraphCountCsv(file, count_col = 2, count_col_name = "ACTIGRAPH_COUNT")
  data = cbind(data, INDEX = 1:nrow(data), EPOCH = epoch, LOCATION = sensorLocation, SR = sr, GRANGE = gr, MPH = speed, WEIGHT = weight, SUBJECT = subj_name, SESSION = session_name, stringsAsFactors = FALSE)
  
  return(data)
}, .parallel = FALSE, .progress = "text", .id = NULL, .inform = TRUE)

devtools::use_data(walkrun1_actigraph, compress = "bzip2", overwrite = TRUE)
