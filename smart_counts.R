args = commandArgs(trailingOnly = TRUE)

if(length(args) != 2){
  stop("Input arguments are not equal to 2")
}else{
  input_file = args[1]
  output_file = args[2]
  print(paste("Input file is:", input_file))
  print(paste("Output file is:", output_file))
  print(paste("Loading file..."))
  dat = mHealthR::mhealth.read(file = input_file, filetype = "sensor")
  print(paste("Computing SMART counts..."))
  output_dat = SMARTcounts::activity_count(df = dat, breaks = "1 min", range = c(-6, 6))
  print(paste("Saving SMART counts..."))
  write.csv(x = output_dat, file = output_file, append = FALSE, quote = FALSE, row.names = FALSE)
  print(paste("Completed"))
}