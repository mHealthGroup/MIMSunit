devtools::load_all(reset=TRUE)
sample_raw_accel_data = import_mhealth_csv('./data-raw/data/sample_input_data.csv')
colnames(sample_raw_accel_data) = c('HEADER_TIME_STAMP', 'X', 'Y', 'Z')
sample_raw_accel_data = sample_raw_accel_data[1:480,]
usethis::use_data(sample_raw_accel_data, overwrite = TRUE)