args = commandArgs(trailingOnly = TRUE)

if(length(args) != 2 || length(args) != 4){
  stop("Input arguments are not equal to 2 or 4")
}else{
  input_file = args[1]
  before_file = args[2]
  after_file = args[3]
  output_file = args[4]
  print(paste("Input file is:", input_file))
  print(paste("Before file is:", before_file))
  print(paste("After file is:", after_file))
  print(paste("Output file is:", output_file))
  print(paste("Loading file..."))
  dat = mHealthR::mhealth.read(file = input_file, filetype = "sensor")
  before_dat = mHealthR::mhealth.read(file = before_file, filetype = "sensor")
  after_dat = mHealthR::mhealth.read(file = after_file, filetype = "sensor")
  print("Get the last 2 minutes data for before file")
  before_dat = mHealthR::mhealth.clip(before_dat, start_time = dat[1,1] - 120, stop_time = dat[1,1], file_type = "sensor")
  print("Get the first 2 minutes data for after file")
  after_dat = mHealthR::mhealth.clip(after_dat, start_time = dat[nrow(dat),1], stop_time = dat[nrow(dat),1] + 120, file_type = "sensor")
  print(paste("Computing SMART counts..."))
  output_dat = SMARTcounts::activity_count(df = dat, breaks = "1 min", range = c(-6, 6), before_df = before_dat, after_df = after_dat)
  print(paste("Saving SMART counts..."))
  write.csv(x = output_dat, file = output_file, append = FALSE, quote = FALSE, row.names = FALSE)
  print(paste("Completed"))
}