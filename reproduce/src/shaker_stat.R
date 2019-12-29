require(plyr)
require(dplyr)

shaker2 = readRDS("reproduce/extdata/shaker2.rds")
shaker3 = readRDS("reproduce/extdata/shaker3.rds")
shaker4 = readRDS("reproduce/extdata/shaker4.rds")

shaker_raw = rbind(shaker2, shaker3, shaker4)

truth_raw = shaker_raw %>% filter(SR == "100" & GRANGE == "16")

shaker_stat = mHealthR::mhealth.extract_characteristics(truth_raw, file_type = "sensor", select_cols = c(2,3,4), group_cols = c("RPM", "LOCATION", "DEVICE", "SR", "GRANGE"), preset = "stat")

shaker_range = shaker_stat %>% select(RPM, COLUMNS, MIN, QUANTILE1, QUANTILE99, MAX) %>% filter(COLUMNS != "Z_ACCELATION_METERS_PER_SECOND_SQUARED") %>% mutate(ABS_MIN = abs(MIN), ABS_QUANTILE1 = abs(QUANTILE1)) %>% select(RPM, QUANTILE99, MAX, ABS_MIN, ABS_QUANTILE1)

shaker_range = shaker_range %>% ddply(c("RPM"), function(seg){
  median(unlist(seg[2:5]))
})
