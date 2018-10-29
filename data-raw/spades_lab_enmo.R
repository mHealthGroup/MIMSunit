require(mHealthR)
require(foreach)
require(reshape2)
require(plyr)
require(dplyr)
require(stringr)
require(doSNOW)
require(MIMSunit)
require(h5)

data_folder = "D:/data/spades_lab_counts_analysis/"

enmoFiles = list.files(path = data_folder, pattern = "*.merged\\.sensorEpoch\\.csv", full.names = TRUE, recursive = TRUE)

enmos = ldply(enmoFiles, function(enmoFile){
  print(paste('process', enmoFile))
  enmo8g_values = import_enmo_csv(filename = enmoFile, col_name = "ENMO8g")
  enmo2g_values = import_enmo_csv(filename = str_replace(enmoFile, '\\.sensorEpoch\\.csv', '_2g\\.sensorEpoch\\.csv'), col_name = 'ENMO2g')
  enmo_values = join(enmo8g_values, enmo2g_values, by="HEADER_TIME_STAMP")
  id = str_extract(enmoFile, 'SPADES_[0-9]+')
  location = strsplit(basename(enmoFile), "_")[[1]][2]
  # import annotation data
  annotationFile = file.path(data_folder, id, "Derived", "SPADESInLab_merged.annotation.csv")
  annotationData = mhealth.read(annotationFile, filetype = "annotation")
  enmo_values = enmo_values %>% adply(1, function(row){
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
  return(enmo_values)
}, .progress = progress_time())

saveRDS(enmos, file = "../MIMSunit/inst/extdata/spades_lab_enmo.rds")
