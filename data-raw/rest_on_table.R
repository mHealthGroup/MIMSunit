devtools::load_all(reset=TRUE)
rest_on_table = import_mhealth_csv('./data-raw/data/rest_on_table.csv')
colnames(rest_on_table) = c('HEADER_TIME_STAMP', 'X', 'Y', 'Z')
usethis::use_data(rest_on_table, overwrite = TRUE)



