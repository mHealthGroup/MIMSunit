require(plyr)
require(dplyr)
require(ggplot2)
require(mHealthR)
require(Counts)
# Read in and clip raw sensor data ----

folder = "../../data/counts/running_maxed_out/"

gt3xFile = list.files(path = folder, all.files = FALSE, full.names = TRUE, pattern = "GT3X.*.csv.*", recursive = FALSE)[[1]]

gt3xbtFile = list.files(path = folder, all.files = FALSE, full.names = TRUE, pattern = "GT3XBT.*.csv.*", recursive = FALSE)[[1]]

gt3xplusFile = list.files(path = folder, all.files = FALSE, full.names = TRUE, pattern = "GT3XPLUS.*.csv.*", recursive = FALSE)[[1]]

gt9xFile = list.files(path = folder, all.files = FALSE, full.names = TRUE, pattern = "GT9X.*.csv.*", recursive = FALSE)[[1]]

gt3xData = mHealthR::mhealth.read(gt3xFile, filetype = "sensor")
gt3xbtData = mHealthR::mhealth.read(gt3xbtFile, filetype = "sensor")
gt3xplusData = mHealthR::mhealth.read(gt3xplusFile, filetype = "sensor")
gt9xData = mHealthR::mhealth.read(gt9xFile, filetype = "sensor")

clipStart = "2014-12-17 11:08:00"
clipEnd = "2014-12-17 11:09:00"
gt3xData = mhealth.clip(gt3xData, start_time = clipStart, stop_time = clipEnd, file_type = "sensor")
gt3xbtData = mhealth.clip(gt3xbtData, start_time = clipStart, stop_time = clipEnd, file_type = "sensor")
gt3xplusData = mhealth.clip(gt3xplusData, start_time = clipStart, stop_time = clipEnd, file_type = "sensor")
gt9xData = mhealth.clip(gt9xData, start_time = clipStart, stop_time = clipEnd, file_type = "sensor")

gt3xbtData_cropped = Counts::crop_grange(gt3xbtData, range = c(-2,2))
gt3xplusData_cropped = Counts::crop_grange(gt3xplusData, range = c(-2,2))

# Save them as Actigraph CSV ----

headStr_gt3x = SensorData.createActigraphCsvHeader(gt3xData$HEADER_TIME_STAMP[1], last(gt3xData$HEADER_TIME_STAMP), samplingRate = 30, sensorId = "MAT2C47090050", firmVersion = "4.4.0", softVersion = "6.11.5")
headStr_gt3xbt = SensorData.createActigraphCsvHeader(gt3xbtData$HEADER_TIME_STAMP[1], last(gt3xbtData$HEADER_TIME_STAMP), samplingRate = 80, sensorId = "MOS2A45130448", firmVersion = "1.3.0", softVersion = "6.11.5")
headStr_gt3xplus = SensorData.createActigraphCsvHeader(gt3xplusData$HEADER_TIME_STAMP[1], last(gt3xplusData$HEADER_TIME_STAMP), samplingRate = 80, sensorId = "CLE2B41120128", firmVersion = "2.2.1", softVersion = "6.11.5")
headStr_gt9x = SensorData.createActigraphCsvHeader(gt9xData$HEADER_TIME_STAMP[1], last(gt9xData$HEADER_TIME_STAMP), samplingRate = 100, sensorId = "TAS1C32140067", firmVersion = "1.1.0", softVersion = "6.11.5")

SensorData.io.writeAsActigraphRaw("offline_data/running_maxed_out/actigraph/", gt3xData, headerStr = headStr_gt3x, custom_name = basename(gt3xFile))
SensorData.io.writeAsActigraphRaw("offline_data/running_maxed_out/actigraph/", gt3xbtData_cropped, headerStr = headStr_gt3xbt, custom_name = basename(gt3xbtFile))
SensorData.io.writeAsActigraphRaw("offline_data/running_maxed_out/actigraph/", gt3xplusData_cropped, headerStr = headStr_gt3xplus, custom_name = basename(gt3xplusFile))
SensorData.io.writeAsActigraphRaw("offline_data/running_maxed_out/actigraph/", gt9xData, headerStr = headStr_gt9x, custom_name = basename(gt9xFile))

# Process using Actilife before running following codes ----

# read back the computed actigraph count values ----
gt3xCounts = import_actigraph_count(paste0("offline_data/running_maxed_out/actigraph/counts/",basename(gt3xFile)))
gt3xbtCounts = import_actigraph_count(paste0("offline_data/running_maxed_out/actigraph/counts/", basename(gt3xbtFile)))
gt3xplusCounts = import_actigraph_count(paste0("offline_data/running_maxed_out/actigraph/counts/", basename(gt3xplusFile)))
gt9xCounts = import_actigraph_count(paste0("offline_data/running_maxed_out/actigraph/counts/", basename(gt9xFile)))

gt3xCountValue = mean(gt3xCounts[,2])
gt3xbtCountValue = mean(gt3xbtCounts[,2])
gt3xplusCountValue = mean(gt3xplusCounts[,2])
gt9xCountValue = mean(gt9xCounts[,2])

gt3xCounts = import_actigraph_count(paste0("offline_data/running_maxed_out/actigraph/counts/",basename(gt3xFile)), axes = c(2))
gt3xbtCounts = import_actigraph_count(paste0("offline_data/running_maxed_out/actigraph/counts/", basename(gt3xbtFile)), axes = c(2))
gt3xplusCounts = import_actigraph_count(paste0("offline_data/running_maxed_out/actigraph/counts/", basename(gt3xplusFile)), axes = c(2))
gt9xCounts = import_actigraph_count(paste0("offline_data/running_maxed_out/actigraph/counts/", basename(gt9xFile)), axes = c(2))

gt3xCountValueX = mean(gt3xCounts[,2])
gt3xbtCountValueX = mean(gt3xbtCounts[,2])
gt3xplusCountValueX = mean(gt3xplusCounts[,2])
gt9xCountValueX = mean(gt9xCounts[,2])

gt3xData = data.frame(gt3xData, SR = 30, GRANGE = 3, TYPE = "GT3X", ACTIGRAPH_COUNT = gt3xCountValue, ACTIGRAPH_COUNT_X = gt3xCountValueX, stringsAsFactors = FALSE)
gt3xbtData = data.frame(gt3xbtData_cropped, SR = 80, GRANGE = 3, TYPE = "GT3XBT", ACTIGRAPH_COUNT = gt3xbtCountValue, ACTIGRAPH_COUNT_X = gt3xbtCountValueX, stringsAsFactors = FALSE)
gt3xplusData = data.frame(gt3xplusData_cropped, SR = 80, GRANGE = 3, TYPE = "GT3XPLUS", ACTIGRAPH_COUNT = gt3xplusCountValue, ACTIGRAPH_COUNT_X = gt3xplusCountValueX, stringsAsFactors = FALSE)
gt9xData = data.frame(gt9xData, SR = 100, GRANGE = 16, TYPE = "GT9X", ACTIGRAPH_COUNT = gt9xCountValue, ACTIGRAPH_COUNT_X = gt9xCountValueX, stringsAsFactors = FALSE)

running_maxed_out = rbind(gt3xData, gt3xbtData, gt3xplusData, gt9xData)

# devtools::use_data(running_maxed_out, compress = "bzip2", overwrite = TRUE)
saveRDS(running_maxed_out, file = "inst/extdata/running_maxed_out.rds", compress = TRUE)