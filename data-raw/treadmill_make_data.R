require(stringr)
require(plyr)
require(dplyr)
require(doSNOW)
require(mHealthR)
require(MIMSunit)

folder = "D:/data/mims_treadmill/data/";
files = list.files(path = folder, pattern = ".*RAW\\.csv", full.names = TRUE, recursive = FALSE)

l_ply(files, function(sensor_file){
  csvData = import_actigraph_csv(sensor_file)
  data2g = MIMSunit::simulate_new_data(oldData = csvData, new_range = c(-2, 2), new_sr = sampling_rate(csvData))
  output_file = str_replace(sensor_file, "RAW\\.csv", "RAW_2g\\.sensor\\.csv")
  options(digits.secs=3)
  options(digits = 6)
  write.csv(data2g, file = output_file, quote = FALSE, row.names = FALSE)
}, .parallel = FALSE, .progress = "text", .inform = TRUE)
