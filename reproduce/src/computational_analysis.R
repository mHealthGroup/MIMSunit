require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)
filename = "F:\\data\\spades_lab\\SPADES_3\\MasterSynced\\2015\\10\\16\\11\\ActigraphGT9X-AccelerationCalibrated-NA.TAS1E23150139-AccelerationCalibrated.2015-10-16-11-00-00-000-M0400.sensor.csv"
treadmill_data = MIMSunit::import_mhealth_csv(filename)
treadmill_data1 = treadmill_data

result = c(0, 0, 0)

for(i in 1:10)
  results1 = system.time(activity_count(treadmill_data1, breaks = "1 min", range = c(-8, 8), noise_level = 0.03, k = 0.05, spar = 0.6, use_resampling = F), gcFirst = T)
  results1 = as.numeric(results1)
  result = result + results1[1:3]

result = result / 10
print(result)