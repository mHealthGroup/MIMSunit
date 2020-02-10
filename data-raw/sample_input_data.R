

sample_raw_accel_data = MIMSunit::import_mhealth_csv('vignettes/data/mhealth3.csv')
colnames(sample_raw_accel_data) = c('HEADER_TIME_STAMP', 'X', 'Y', 'Z')
usethis::use_data(sample_raw_accel_data, overwrite = TRUE)