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



ldply(sensorFiles, function(sensorFile){
  if(str_detect(sensorFile, '2g')){
    range = 2
  }else{
    range = 8
  }
  if(str_detect(sensorFile, '(SPADES_31)|(SPADES_38)')){
    sr = 100
  }else{
    sr = 80
  }
  print(paste('processing', sensorFile))
  cmd = paste('C:/tools/miniconda3/envs/research-py27/python',
              'C:\\Users\\tqshe\\Projects\\python\\enmo-algorithm\\ActivitySummary.py',
              paste('\"', sensorFile, '\"', sep = ''),
              '-range',
              range,
              '-sr',
              sr
              )
  system(cmd)
}, .parallel = FALSE, .progress = progress_text())
