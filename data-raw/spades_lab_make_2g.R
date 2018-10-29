require(mHealthR)
require(foreach)
require(reshape2)
require(plyr)
require(dplyr)
require(stringr)
require(doSNOW)
require(MIMSunit)

data_folder = "D:/data/spades_lab_counts_analysis"

sensorFiles = list.files(path = data_folder, pattern = "*.sensor\\.csv$", full.names = TRUE, recursive = TRUE)

# choose only waist and nondominant wrist
mask = str_detect(sensorFiles, '(_dominant waist_)|(_non dominant wrist_)')
mask2 = str_detect(sensorFiles, 'SPADES_38')
sensorFiles = sensorFiles[mask & mask2]

options(digits.secs=3)

ldply(sensorFiles, function(sensorFile){

  sensorData = mhealth.read(sensorFile, filetype='sensor')
  sensorData_2g = simulate_new_data(sensorData, c(-2,2), sampling_rate(sensorData))
  sensorData_2g[,1] = strftime(sensorData_2g[,1], '%Y-%m-%d %H:%M:%OS')
  output = str_replace(sensorFile, pattern = '\\.sensor\\.csv', replacement = '_2g\\.sensor\\.csv')

  write.csv(x = sensorData_2g, file = output, quote = FALSE, row.names = FALSE)


}, .parallel = FALSE, .progress = progress_text())
