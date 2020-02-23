devtools::load_all(reset=TRUE)
edge_case = import_mhealth_csv('./data-raw/data/edge_case.csv')
colnames(edge_case) = c('HEADER_TIME_STAMP', 'X', 'Y', 'Z')
usethis::use_data(edge_case, overwrite = TRUE)