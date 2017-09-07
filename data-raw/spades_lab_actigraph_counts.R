require(mHealthR)
require(foreach)
require(reshape2)
require(plyr)
require(dplyr)
require(stringr)
require(doSNOW)
require(Counts)
require(h5)

data_folder = "F:/data/spades_lab_counts_analysis/"

# create actigraph count data package
count_folder = "F:/data/spades_lab_counts_analysis/Actigraph_raw/agd"

countFiles = list.files(path = count_folder, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

actigraphCounts = ldply(countFiles, function(countFile){
  actigraphCounts = import_actigraph_count(filename = countFile)
  id = paste(strsplit(basename(countFile), "_")[[1]][1], strsplit(basename(countFile), "_")[[1]][2], sep="_")
  location = strsplit(basename(countFile), "_")[[1]][3]
  # import annotation data
  annotationFile = file.path(data_folder, id, "Derived", "SPADESInLab_merged.annotation.csv")
  annotationData = mhealth.read(annotationFile, filetype = "annotation")
  actigraphCounts = actigraphCounts %>% adply(1, function(row){
    st = row$HEADER_TIME_STAMP
    et = st + 5
    selected_annotations = mhealth.clip(annotationData, st, et, file_type = "annotation")
    annotation_names = tolower(paste0(selected_annotations$LABEL_NAME, collapse=" "))
    if(any(as.numeric(selected_annotations$STOP_TIME- selected_annotations$START_TIME) < 5)){
      return(NULL)
    }else{
      if(str_detect(annotation_names, "lying")){
        activity = "Lying"
      }else if(str_detect(annotation_names, "(sit)|(reclin)")){
        activity = "Sitting"
      }else if(str_detect(annotation_names, "sweep")){
        activity = "Sweeping"
      }else if(str_detect(annotation_names, "laundry")){
        activity = "Laundry"
      }else if(str_detect(annotation_names, "frisbee")){
        activity = "Frisbee"
      }else if(str_detect(annotation_names, "shelf")){
        activity = "Reload/unload shelf"
      }else if(str_detect(annotation_names, "stairs") & str_detect(annotation_names, "up")){
        activity = "Upstairs"
      }else if(str_detect(annotation_names, "stairs") & str_detect(annotation_names, "down")){
        activity = "Downstairs"
      }else if(str_detect(annotation_names, "stand|still")){
        activity = "Standing"
      }else if(str_detect(annotation_names, "(1mph)|(1 mph)")){
        activity = "Walk 1 mph"
      }else if(str_detect(annotation_names, "(2mph)|(2 mph)")){
        activity = "Walk 2 mph"
      }else if(str_detect(annotation_names, "(3mph)|(3 mph)")){
        activity = "Walk 3 mph"
      }else if(str_detect(annotation_names, "(3.5mph)|(3.5 mph)")){
        activity = "Walk 3.5 mph"
      }else if(str_detect(annotation_names, "walk") & str_detect(annotation_names, "(city)|(outdoor)")){
        activity = "Self-paced walk"
      }else if(str_detect(annotation_names, "(run)|jog")){
        activity = "Run- 5.5 mph"
      }else if(str_detect(annotation_names, "biking") & str_detect(annotation_names, "stationary")){
        activity = "Bike- 300 kmph/min"
      }else if(str_detect(annotation_names, "biking") & str_detect(annotation_names, "(outdoor)|(city)")){
        activity = "Bike outdoor"
      }else{
        return(NULL)
      }
    }
    row$LABEL_NAME = activity
    row$PID = as.numeric(str_split(id, "_")[[1]][2])
    row$LOCATION = location
    return(row)
  })
  return(actigraphCounts)
}, .progress = progress_time())

saveRDS(actigraphCounts, file = "../counts/inst/extdata/spades_lab_actigraph_counts.rds")
