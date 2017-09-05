# Prepare SPADES lab data for counts analysis
require(mHealthR)
require(foreach)
require(ggplot2)
require(reshape2)
require(plyr)
require(stringr)
require(doSNOW)
cl = makeCluster(4)
registerDoSNOW(cl)
sensorLocationFile = "Sensor_location_Lab.csv";
subjectFile = "Subject.csv";
sessionsFile = "Sessions.csv";
sessionDateFormat = "%m/%d/%Y %H:%M";
epoch = 5;

summarizeParticipantInfo = function(subjects){
  subjData = ldply(subjects, function(subj){
    # Read in subject file
    subjInfo = read.csv(file = file.path(from, subj, subjectFile), header = TRUE, as.is = TRUE, stringsAsFactors = FALSE)
    print(paste("Process", subj))
    if(is.na(subjInfo[1,3])){
      subjInfo[1,3] = "NA"
    }
    if(subjInfo[1,3] == FALSE){
      subjInfo[1,3] = "F"
    }
    return(data.frame(ID = subj, AGE = subjInfo[1, 2], GENDER = subjInfo[1, 3], WEIGHT = subjInfo[1, 5], HEIGHT = subjInfo[1,6], DOMINANT_HAND = toupper(str_sub(subjInfo[7], 1,1)), PHONE_MODEL = str_trim(subjInfo$MOBILE_PHONE_MODEL[1]), PHONE_LOCATION = subjInfo$PHONE_LOCATION[1], SHOE_TYPE = toupper(str_trim(subjInfo$SHOE_TYPE[1]))))
  }, .progress = progress_text(), .inform = TRUE)
  return(subjData)
}

computeCounts = function(subjects, input_folder, output_folder, range){
  for(subj in subjects){
    print(paste("Compute activity counts for ", subj))
    sensorFiles = normalizePath(list.files(path = file.path(input_folder), pattern = paste0(subj, "_TAS.*.RData"), full.names = TRUE, recursive = TRUE))
    # summarize sensors
    l_ply(sensorFiles, function(sensorFile){
      k = 0.05
      spar = 0.6
      noise_level = 0.03
      epoch = 5
      require(stringr)
      sensorId = str_split(basename(sensorFile), "_")[[1]][3]
      sensorLocation = str_split(basename(sensorFile), "_")[[1]][4]

      print(paste("Read in", sensorId, "at", sensorLocation))

      if(subj == "SPADES_7" && str_detect(sensorLocation, "^non dominant wrist*")){
        print("This sensor is malfunctioning, skip")
      }else{

      load(sensorFile)

      count = activity_count(sensorData, breaks = paste(epoch, "sec"),
                                                range = range,
                                                noise_level = noise_level,
                                                k = k,
                                                spar = spar,
                                                filter_type = "butter",
                                                cutoffs = c(0.2, 5),
                                                integration = "trapz")
      dir.create(file.path(to_merge, "stat"), recursive = TRUE)
      SensorData.io.write(file.path(to_merge, output_folder), count,
                          custom_name = paste(paste(subj, sensorId, sensorLocation, sep = "_"),"count", "csv", sep = "."), append = FALSE,
                          header = TRUE,
                          gzip = FALSE,
                          flatDir = TRUE,
                          splitHour = FALSE)
      }
    }, .parallel = FALSE, .progress = progress_text())
  }
}

computeStats = function(subjects, input_folder, output_folder){
  for(subj in subjects){

    print(paste("Compute stats and characteristics for ", subj))

    sensorFiles = normalizePath(list.files(path = file.path(input_folder), pattern = paste0(subj, "_TAS.*.RData"), full.names = TRUE, recursive = TRUE))
    # summarize sensors
    l_ply(sensorFiles, function(sensorFile){
      require(mHealthR)
      require(stringr)
      sensorId = str_split(basename(sensorFile), "_")[[1]][3]
      print(paste("Read in ", sensorId))
      load(sensorFile)
      segmentedData = mHealthR::mhealth.segment(sensor_data, breaks = "5 sec", file_type = "sensor")
      statsData = mHealthR::mhealth.extract_characteristics(segmentedData, file_type = "sensor", select_cols = c(2,3,4), group_cols = c("SEGMENT"), preset = "primary")
      statsData[7:ncol(statsData)] = round(statsData[7:ncol(statsData)], digits = 4)
      dir.create(output_folder, recursive = TRUE)
      write.csv(x = statsData, file = file.path(output_folder, paste(paste(subj, sensorId, sep = "_"),"stat", "csv", sep = ".")), append = FALSE, row.names = FALSE, quote = FALSE)
    }, .parallel = FALSE, .progress = progress_text())
  }
}

extractActivities = function(subjects){
  for (subj in subjects) {
    print(paste('extra activities of', subj))
    folder = file.path(to, subj, ready_folder)
    annFile = list.files(path = folder, pattern = "SPADESInLab.*annotation.csv.*", full.names = TRUE, recursive = TRUE)
    if(is.vector(annFile)){
      annFile = annFile[1]
    }
    annotations = AnnotationData.importCsv(annFile)
    annotations = AnnotationData.simplify(annotations)
    # select activities
    markedActivities = list("lying" = c("Lying"),
                            "sitting" = c("Sitting", "not writing", "not web browsing"),
                            "sitting web browsing" = c("sitting", "web browsing"),
                            "sitting writing" = c("sitting", "writing"),
                            "standing" = c("Standing", "not laundry", "not shelf unload", "not shelf reload", "not sweeping", "not writing", "not web browsing"),
                            "standing web browsing" = c("standing", "web browsing"),
                            "standing writing" = c("standing", "writing"),
                            "walking at 1mph arms on desk" = c("Walking", "Treadmill", "1 mph"),
                            "walking at 2mph arms on desk" = c("Walking", "Treadmill", "2 mph"),
                            "walking at 3mph" = c("Walking", "Treadmill", "3 mph", "not Phone Talking", "not carrying drink", "not bag"),
                            "walking at 3mph carrying drink" = c("Walking or 3 mph", "Treadmill", "Carrying Drink"),
                            "walking at 3mph carrying bag" = c("Walking or 3 mph", "Treadmill", "bag"),
                            "walking at 3mph phone talking" = c("Walking or 3 mph", "Treadmill", "Phone Talking or telling story"),
                            "walking at 3.5mph" = c("Walking", "3.5 mph", "Treadmill"),
                            "running at 5.5mph 5% grade" = c("Jog/Running or 5.5 mph"),
                            "walking outdoor" = c("Walking", "Outdoors or City"),
                            "walking upstairs" = c("Walking", "Stairs", "Up"),
                            "walking donwstairs" = c("Walking", "Stairs", "Down"),
                            "biking at 300 KPM/Min" = c("Biking", "Stationary or 300 KPM/Min"),
                            "biking outdoor" = c("Biking", "Outdoors or City"),
                            "sweeping" = c("Sweeping"),
                            "laundry" = c("Laundry"),
                            "shelf unload" = c("shelf unload", "not shelf reload"),
                            "shelf reload" = c("shelf reload", "not shelf unload"),
                            "frisbee" = c("Frisbee")
    )
    activities = ldply(markedActivities, function(labels){
      or_labels = str_detect(labels, " or ")
      not_labels = str_detect(labels, "not ")
      if (sum(or_labels) > 0) {
        or = str_split(labels[or_labels], " or ")
      }else{
        or = c()
      }
      if(sum(not_labels) > 0) {
        not = str_split(labels[not_labels], "not ")
      }else{
        not = c()
      }
      and = labels[!or_labels & !not_labels]

      sub_ann = AnnotationData.simplify.filter(annotations, labels = and, include.labels = TRUE, and.logic = TRUE)
      if (length(or) != 0) {
        for(oo in or){
          sub_ann = AnnotationData.simplify.filter(sub_ann, labels = oo, include.labels = TRUE, and.logic = FALSE)
        }
      }
      if (length(not) != 0){
        for(nn in not){
          sub_ann = AnnotationData.simplify.filter(sub_ann, labels = nn[2], include.labels = FALSE, and.logic = TRUE)
        }
      }
      # connect adjacent annotations
      i = 1
      sub_ann_connected = data.frame()
      while(i <= nrow(sub_ann)){
        if(i == nrow(sub_ann)){ sub_ann_connected = data.frame(HEADER_TIME_STAMP = sub_ann[i, 1],
                                                               START_TIME = sub_ann[i, 2],
                                                               STOP_TIME = sub_ann[i, 3],
                                                               LABEL_NAME = paste0('"',sub_ann[i, 4], '"'), stringsAsFactors = FALSE); break;}
        j = i
        while(sub_ann[i, 3] == sub_ann[i+1,2]){
          i = i + 1
          if(i == nrow(sub_ann)) break;
        }
        label_name = paste0('"', paste(unique(unlist(str_split(sub_ann[j:i, 4], ","))), collapse = ","), '"')
        sub_ann_connected = rbind(sub_ann_connected, data.frame(HEADER_TIME_STAMP = sub_ann[j, 1],
                                            START_TIME = sub_ann[j, 2],
                                            STOP_TIME = sub_ann[i, 3],
                                            LABEL_NAME = label_name, stringsAsFactors = FALSE))
        if(i == nrow(sub_ann)){ break; }
        else{
          i = i + 1
          }
      }

      # exclude annotations that are less than 5 seconds
      sub_ann_connected = adply(sub_ann_connected, 1, function(row){
        if(as.numeric(row[[3]] - row[[2]], units = "secs") < 5){
          return(NULL)
        }else{
          return(row)
        }
      })
      return(sub_ann_connected)
    })
    names(activities)[1] = "ACTIVITY_NAME"
    activities = activities[c(1,3,4,5)]
    dir.create(file.path(to_merge, "activities"))
    AnnotationData.io.write(file.path(to_merge, "activities"), activities, custom_name = paste(subj, str_replace(basename(annFile), "annotation.*", "activity.csv"),sep = "_"), gzip = FALSE, flatDir = TRUE, splitHour = FALSE, append = FALSE, header = TRUE)
  }
}

.extractCountsGivenDuration = function(counts, startTime, endTime){
  counts[,1] = lubridate::floor_date(counts[,1])
  selected_counts = SensorData.clip(counts, startTime = startTime, endTime = endTime)
  if(nrow(selected_counts) > 6){
    selected_counts = selected_counts[3:(nrow(selected_counts)-2),]
  }else if(nrow(selected_counts) > 3){
    selected_counts = selected_counts[2:(nrow(selected_counts)-1),]
  }else{
    selected_counts = selected_counts[0, ]
  }
  return(selected_counts)
}

getCountsPerActivities = function(subjects, locations = c("dominant wrist", "dominant waist")){
  final_result = data.frame()
  for(subj in subjects){
    print(paste("process", subj))
    actFile = list.files(path = file.path(to_merge, "activities"), pattern = paste0(subj,"_SPADESInLab.*activity.*"), full.names = TRUE, recursive = FALSE)
    if(length(actFile) == 0){
      print(paste("Not found activity file:", subj))
      next;
    }else{
      actFile = actFile[[1]]
      activities = read.csv(actFile, quote = '"', stringsAsFactors = FALSE)
      activities[,2] = as.POSIXct(activities[,2], format = "%Y-%m-%d %H:%M:%OS")
      activities[,3] = as.POSIXct(activities[,3], format = "%Y-%m-%d %H:%M:%OS")
    }

    ownCounts = list()
    actiCounts = list()
    maxedoutCounts = list()
    actiMaxedoutCounts = list()
    for(loc in locations){
      if(subj == "SPADES_7" && loc == "non dominant wrist"){
        next
      }
      if(subj == "SPADES_32" && loc == "dominant ankle"){
        next
      }
      # our own counts
      ownCountsFile = list.files(path = file.path(to_merge, "own_count"), pattern = paste0(subj, "_TAS.*_", loc, "\\.count\\.csv"), full.names = TRUE, recursive = FALSE)

      if(length(ownCountsFile) == 0){
        print(paste("Not found own counts:", subj, ",", loc))
      }else{
        ownCountsFile = ownCountsFile[[1]]
        ownCounts[[loc]] = read.csv(ownCountsFile, stringsAsFactors = FALSE)
        ownCounts[[loc]][,1] = as.POSIXct(ownCounts[[loc]][,1], format = "%Y-%m-%d %H:%M:%OS")
        ownCounts[[loc]] = na.omit(ownCounts[[loc]])
      }

      # actigraph counts
      actiCountsFile = list.files(path = file.path(to_merge, "agd_count"), pattern = paste0(subj, "_TAS.*_", loc, "_merged\\.actigraph5sec.*"), full.names = TRUE, recursive = FALSE)
      if(length(actiCountsFile) == 0){
        print(paste("Not found actigraph counts:", subj, ",", loc))
      }else{
        actiCountsFile = actiCountsFile[[1]]
        actiCounts[[loc]] = SensorData.importActigraphCountCsv(actiCountsFile, count_col_name = "ACTIGRAPH")
      }

      n_common = min(nrow(ownCounts[[loc]]), nrow(actiCounts[[loc]]))
      ownCounts[[loc]] = ownCounts[[loc]][1:n_common,]
      actiCounts[[loc]] = actiCounts[[loc]][1:n_common,]

      if(as.numeric(ownCounts[[loc]][1,1] - actiCounts[[loc]][1,1], unit = "secs") > 1){
        ownCounts[[loc]][,1] = actiCounts[[loc]][,1]
        write.csv(ownCounts[[loc]], ownCountsFile, quote = FALSE, row.names = FALSE)
      }

      # maxedout counts
      maxedoutCountsFile = list.files(path = file.path(to_merge, "own_maxedout_count"), pattern = paste0(subj, "_TAS.*_", loc, "\\.count.*"), full.names = TRUE, recursive = FALSE)

      if(length(maxedoutCountsFile) == 0){
        print(paste("Not found maxedout counts:", subj, ",", loc))
      }else{
        maxedoutCountsFile = maxedoutCountsFile[[1]]
        maxedoutCounts[[loc]] = read.csv(maxedoutCountsFile, stringsAsFactors = FALSE)
        maxedoutCounts[[loc]][,1] = as.POSIXct(maxedoutCounts[[loc]][,1], format = "%Y-%m-%d %H:%M:%OS")
        maxedoutCounts[[loc]] = na.omit(maxedoutCounts[[loc]])
      }

      # maxedout actigraph counts
      actiMaxedoutCountsFile = list.files(path = file.path(to_merge, "agd_maxedout_count"), pattern = paste0(subj, "_TAS.*_", loc, ".*maxedout\\.actigraph5sec.*"), full.names = TRUE, recursive = FALSE)

      if(length(actiMaxedoutCountsFile) == 0){
        print(paste("Not found actigraph maxedout counts:", subj, ",", loc))
      }else{
        actiMaxedoutCountsFile = actiMaxedoutCountsFile[[1]]
        actiMaxedoutCounts[[loc]] = SensorData.importActigraphCountCsv(actiMaxedoutCountsFile, count_col_name = "ACTIGRAPH_MAXEDOUT")
      }

      n_common = min(nrow(maxedoutCounts[[loc]]), nrow(actiMaxedoutCounts[[loc]]))
      maxedoutCounts[[loc]] = maxedoutCounts[[loc]][1:n_common,]
      actiMaxedoutCounts[[loc]] = actiMaxedoutCounts[[loc]][1:n_common,]

      if(as.numeric(maxedoutCounts[[loc]][1,1] - actiMaxedoutCounts[[loc]][1,1], unit = "secs") > 1){
        maxedoutCounts[[loc]][,1] = actiMaxedoutCounts[[loc]][,1]
        write.csv(maxedoutCounts[[loc]], maxedoutCountsFile, quote = FALSE, row.names = FALSE)
      }
    }

    countsPerActivity = adply(activities, 1, function(row){
      st = row[[2]]
      et = row[[3]]
      result = data.frame()
      for(loc in locations){
        if(subj == "SPADES_7" & loc == 'non dominant wrist'){
          print(subj)
        }
        if(length(ownCounts[[loc]]) > 0){
          selected_counts_own = .extractCountsGivenDuration(ownCounts[[loc]], st, et)
        }else{
          selected_counts_own = data.frame()
        }
        if(length(actiCounts[[loc]] > 0)){
          selected_counts_actigraph = .extractCountsGivenDuration(actiCounts[[loc]], st, et)
        }else{
          selected_counts_actigraph = data.frame()
        }
        if(length(maxedoutCounts[[loc]] > 0)){
          selected_counts_maxedout_own = .extractCountsGivenDuration(maxedoutCounts[[loc]], st, et)
        }else{
          selected_counts_maxedout_own = data.frame()
        }
        if(length(actiMaxedoutCounts[[loc]] > 0)){
          selected_counts_maxedout_acti = .extractCountsGivenDuration(actiMaxedoutCounts[[loc]], st, et)
        }else{
          selected_counts_maxedout_acti = data.frame()
        }

        if(nrow(selected_counts_actigraph) > 0){
          if(!all(selected_counts_actigraph[,1] == selected_counts_actigraph[,1])){
            print(paste("rerun actilife on this subject:", subj, ",", loc))
          }
        }

        if(nrow(selected_counts_own) != nrow(selected_counts_actigraph)){
          print(subj)
          print(loc)
        }

        if(nrow(selected_counts_own) > 0){
          result_own = data.frame(row, value = selected_counts_own[,2], TYPE = "COUNTS", LOCATION = toupper(loc),stringsAsFactors = FALSE)
          result = rbind(result, result_own)
        }
        if(nrow(selected_counts_actigraph) > 0){
          result_actigraph = data.frame(row, value = selected_counts_actigraph[,2], TYPE = "ACTIGRAPH", LOCATION = toupper(loc), stringsAsFactors = FALSE)
          result = rbind(result, result_actigraph)
        }
        if(nrow(selected_counts_maxedout_own) > 0){
          result_maxedout_own = data.frame(row, value = selected_counts_maxedout_own[,2], TYPE = "COUNTS_MAXEDOUT", LOCATION = toupper(loc),stringsAsFactors = FALSE)
          result = rbind(result, result_maxedout_own)
        }

        if(nrow(selected_counts_maxedout_acti) > 0){
          result_maxedout_acti = data.frame(row, value = selected_counts_maxedout_acti[,2], TYPE = "ACTIGRAPH_MAXEDOUT", LOCATION = toupper(loc),stringsAsFactors = FALSE)
          result = rbind(result, result_maxedout_acti)
        }
      }
      return(result)
    })
    countsPerActivity = data.frame(countsPerActivity, SUBJECT = subj, stringsAsFactors = FALSE)
    final_result = rbind(final_result, countsPerActivity)
  }
  return(final_result)
}


getStatsPerActivities = function(subjects, locations = c("non dominant wrist", "dominant hip"), input_folder){
  require(spades)
  statsData = data.frame()
  for(subj in subjects){
    print(paste("process", subj))
    subj_id = as.numeric(str_split(subj, "_")[[1]][2])
    # ---- load activity file for subject ----
    activityFile = list.files(input_folder, pattern = paste0(subj, "_.*\\.activity\\.csv"), full.names = TRUE, recursive = TRUE)
    if(length(activityFile) == 0){
      print(paste("Not found activity file: ", subj))
      activities = data.frame()
    }else{
      activityFile = activityFile[[1]]
      activities = read.csv(activityFile, stringsAsFactors = FALSE, header = TRUE)
    }

    # ---- load stat file for subject and locations ----
    for(loc in locations){
      sensor_id = spades::map_location_to_sid(subj_id, locations = loc)
      statsFile = list.files(input_folder, pattern = paste0(subj, "_", sensor_id, ".*\\.stat\\.csv"), full.names = TRUE, recursive = TRUE)
      if(length(statsFile) == 0){
        print(paste("Not found stats file:", subj, ",", loc))
      }else{
        statsFile = statsFile[[1]]
        tmpData = read.csv(statsFile, stringsAsFactors = FALSE)
        tmpData = mhealth.convert(tmpData, file_type = "feature", required_cols = 3:20, group_cols = 1:2, datetime_format = mhealth$format$csv$TIMESTAMP, timezone = Sys.timezone())
        statsPerActivity = adply(activities, 1, function(row){
          st = row[[2]]
          et = row[[3]]
          result = data.frame()
          if(subj == "SPADES_7" & loc == 'non dominant wrist'){
            print("skip malfunctional sensor")
          }
          if(length(tmpData) > 0){
            selected_data = mhealth.clip(tmpData, start_time = st, stop_time = et, file_type = "feature")
          }else{
            selected_data = data.frame()
          }
          if(nrow(selected_data) > 0){
            selected_data = data.frame(selected_data, LOCATION = toupper(loc), SUBJ = subj, stringsAsFactors = FALSE)
            result = rbind(result, selected_data)
          }
          return(result)
        })
        statsData = rbind(statsData, statsPerActivity)
      }
    }
  }
  return(statsData)
}

ids = seq(1, 51)
# ids = c(35)
# ids = c(7, 43, 45, 47, 48, 49, 50)
# ids = c(4)

exclude_ids = c(5, 33)
# 5 doesn't exist
# 33 has corrupted annotation files
# 4, 6, 7, 8, 9 needs to check and fix the sensor mapping

# annotations
# actFile_ids = c(4,6,7,8,9)
# actFile_ids = setdiff(actFile_ids, exclude_ids)
# actFile_subjects = paste("SPADES", actFile_ids, sep = "_")
# mergeSpadesLabAnnotationFiles(actFile_subjects)
# extractActivities(actFile_subjects)

# sensors
# sensor_ids = c(7)
# sensor_ids = setdiff(sensor_ids, exclude_ids)
sensor_ids = setdiff(ids, exclude_ids)
# sensor_ids = c(31, 32)
sensor_subjects = paste("SPADES", sensor_ids, sep = "_")
# extractActivities(sensor_subjects)
computeCounts(sensor_subjects, "../../data/SPADES_LAB_MERGED/data/", "../../data/SPADES_LAB_MERGED/SMART_count", c(-8,8))
computeCounts(sensor_subjects, "../../data/SPADES_LAB_MERGED/data/", "own_maxedout_count", c(-2, 2))
# computeStats(sensor_subjects, "../../data/SPADES_LAB_MERGED/data/", "../../data/SPADES_LAB_MERGED/stats/")
# subjData = summarizeParticipantInfo(sensor_subjects)
# stopCluster(cl)

# overall
# ids = setdiff(ids, exclude_ids)
# subjects = paste("SPADES", ids, sep = "_")
# spades_lab_stats = getStatsPerActivities(subjects = sensor_subjects, locations = c("non dominant wrist", "dominant hip"), input_folder = "../../data/SPADES_LAB_MERGED/")
# spades_lab_counts = getCountsPerActivities(sensor_subjects, locations = c("dominant wrist", "dominant waist", "non dominant wrist", "non dominant waist", "dominant ankle", "non dominant ankle"))
# devtools::use_data(spades_lab_counts, compress = "bzip2", overwrite = TRUE)
# saveRDS(spades_lab_counts, file = "inst/extdata/spades_lab_counts.rds", compress = TRUE)
#
# devtools::use_data(spades_lab_stats, compress = "bzip2", overwrite = TRUE)
saveRDS(spades_lab_stats, file = "inst/extdata/spades_lab_stats.rds", compress = TRUE)




