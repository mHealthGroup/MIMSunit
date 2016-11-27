# create adaptive sampling rate example for mhealth package
# 
rm(list = ls(all.names = TRUE))
require(stringr)
require(plyr)
require(dplyr)

folder = "offline_data/adaptive_sampling_rate/";

# shaker raw data -----------
files = list.files(path = folder, all.files = FALSE, full.names = TRUE, pattern = ".*.csv.*", recursive = TRUE)
file = normalizePath(unlist(files))[[1]]

adaptive_sampling_rate = file %>% SensorData.importCsv %>% SamplingRate.summary(breaks = "sec")

# save as exported data
# devtools::use_data(adaptive_sr, compress = "bzip2", overwrite = TRUE)
saveRDS(adaptive_sampling_rate, file = "inst/extdata/adaptive_sampling_rate.rds", compress = TRUE)
