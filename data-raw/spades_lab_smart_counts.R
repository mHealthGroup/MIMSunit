require(mHealthR)
require(foreach)
require(reshape2)
require(plyr)
require(dplyr)
require(stringr)
require(doSNOW)
require(Counts)

spar = 0.6
k = 0.05
noise_level = 0.03
epoch = 5

data_folder = "F:/data/spades_lab_counts_analysis"

sensorFiles = list.files(path = data_folder, pattern = "*.h5$", full.names = TRUE, recursive = TRUE)

# choose only waist and nondominant wrist
mask = str_detect(sensorFiles, '(_dominant waist_)|(_non dominant wrist_)')
sensorFiles = sensorFiles[mask]

smartCounts = ldply(sensorFiles, function(sensorFile){
  id = basename(dirname(dirname(sensorFile)))
  location = str_split(basename(sensorFile), "_")[[1]][2]

  # check cache
  cache_path = file.path(data_folder, paste(id, location, "counts.csv", sep = "_"))
  if(file.exists(cache_path)){
    smartCounts = read.csv(file = cache_path, stringsAsFactors = FALSE)
    smartCounts$HEADER_TIME_STAMP = as.POSIXct(smartCounts$HEADER_TIME_STAMP, tz="CEST")
    return(smartCounts)
  }else{
    # import data
    sensorData = import_hdf5(sensorFile, "sensor")
    maxedout_sensorData = make_sensor_data(sensorData, c(-2,2), sampling_rate(sensorData))
    smartCounts = activity_count(sensorData, breaks = paste(epoch, "sec"),
                           range = c(-8, 8),
                           noise_level = noise_level,
                           k = k,
                           spar = spar)
    colnames(smartCounts)[2] = "SMARTcounts8g"
    maxedout_smartCounts = activity_count(maxedout_sensorData, breaks = paste(epoch, "sec"),
                                 range = c(-2, 2),
                                 noise_level = noise_level,
                                 k = k,
                                 spar = spar)
    colnames(maxedout_smartCounts)[2] = "SMARTcounts2g"
    countsData = join(smartCounts, maxedout_smartCounts, by='HEADER_TIME_STAMP')

    # import annotation data
    annotationFile = file.path(dirname(sensorFile), "SPADESInLab_merged.annotation.csv")
    annotationData = mhealth.read(annotationFile, filetype = "annotation")
    countsData = countsData %>% adply(1, function(row){
      st = row$HEADER_TIME_STAMP
      et = st + 5
      selected_annotations = mhealth.clip(annotationData, st, et, file_type = "annotation")
      annotation_names = tolower(paste0(selected_annotations$LABEL_NAME, collapse=" "))
      print(annotation_names)
      if(any(as.numeric(selected_annotations$STOP_TIME- selected_annotations$START_TIME) < 5)){
        return(NULL)
      }else{
        if(str_detect(annotation_names, "biking")){
          print(annotation_names)
        }
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
        }else if(str_detect(annotation_names, "walk") & str_detect(annotation_names, "(city)|(outdoors)")){
          activity = "Self-paced walk"
        }else if(str_detect(annotation_names, "(run)|jog")){
          activity = "Run- 5.5 mph"
        }else if(str_detect(annotation_names, "biking") & str_detect(annotation_names, "stationary")){
          activity = "Bike- 300 kmph/min"
        }else if(str_detect(annotation_names, "biking") & str_detect(annotation_names, "outdoors")){
          activity = "Bike outdoor"
        }else{
          return(NULL)
        }
      }
      row$LABEL_NAME = activity
      row$PID = as.numeric(str_split(id, "_")[[1]][2])
      row$LOCATION = location
      return(row)
    }, .progress=progress_text())
    write.csv(countsData, row.names = FALSE, col.names = TRUE, quote = FALSE, file = file.path(data_folder, paste0(id, "_", location, "_counts.csv")))
    return(countsData)
  }

}, .parallel = FALSE, .progress = progress_text())

saveRDS(smartCounts, file = "inst/extdata/spades_lab_smart_counts.rds")