# create exported dataset for mhealth package
#
rm(list = ls(all.names = TRUE))
require(stringr)
require(plyr)
require(dplyr)
require(mHealthR)
require(SMARTcounts)

folder = "F:/data/shaker5/";

# shaker raw data -----------
files = list.files(path = folder, all.files = FALSE, full.names = TRUE, pattern = ".*.csv", recursive = FALSE)
files = files[!str_detect(files, "sessions")]
files = normalizePath(unlist(files))

shaker5 = ldply(files, function(file){
  header = file %>% import_actigraph_meta(header = TRUE)
  sr = header$sr
  if(str_detect(file, 'ActivPal')){
    id = "ActivPal3"
    gr = 2
  }else{
    id = header$sn
    gr = header$gr
  }

  raw_data = import_actigraph_raw(file, ad_convert = FALSE, ts_provided = TRUE, header_provided = TRUE)
  # write.csv(raw_data, file = file.path(folder, 'enmo', basename(file)), quote = FALSE, row.names = FALSE)
  # cut into segments
  sessions = read.csv(file.path(folder, "sessions.csv"), header = TRUE, stringsAsFactors = FALSE)
  segmented = sessions %>% adply(1, function(row){
    segment = raw_data %>% mhealth.clip(row[,1], row[,2], file_type = 'sensor')
    rpm = row[,3]
    hz = row[,4]
    return(segment %>% cbind(LOCATION = "Shaker", SR = sr, RPM = hz, DEVICE = id, GRANGE = gr, stringsAsFactors = FALSE))
  }, .inform = TRUE, .id = NULL) %>%
    subset(select = -(1:3))
  return(segmented)
}, .parallel = FALSE, .progress = "text", .id = NULL, .inform = TRUE)

columns = c("X_ACCELATION_METERS_PER_SECOND_SQUARED","Y_ACCELATION_METERS_PER_SECOND_SQUARED","Z_ACCELATION_METERS_PER_SECOND_SQUARED")

colnames(shaker5)[2:4] = columns

# save as external data
# devtools::use_data(shaker3, compress = "bzip2", overwrite = TRUE)
saveRDS(shaker5, file = "inst/extdata/shaker5.rds", compress = TRUE)
