require(plyr)
require(dplyr)
require(stringr)
require(ggplot2)
require(reshape2)
require(extrafont)

mims_unit_filename = "reproduce/extdata/spades_lab_mims_unit.rds"
spades_lab_mims_unit = readRDS(mims_unit_filename)
actigraph_counts_filename = "reproduce/extdata/spades_lab_actigraph_counts.rds"
spades_lab_actigraph_counts = readRDS(actigraph_counts_filename)
spades_lab_actigraph_counts$PID = as.numeric(spades_lab_actigraph_counts$PID)
enmo_filename = "reproduce/extdata/spades_lab_enmo.rds"
spades_lab_enmos = readRDS(enmo_filename)
spades_lab_enmos$PID = as.numeric(spades_lab_enmos$PID)

# merge these two dataframes
spades_lab_counts = plyr::join(spades_lab_mims_unit, plyr::join(spades_lab_actigraph_counts, spades_lab_enmos))
spades_lab_counts = na.omit(spades_lab_counts)


loadfonts(device = 'win')

acc_factor = 60 / 5

# 51 participants' average, compared with Actigraph and METs
act_list = c("lying",
             "sitting",
             "sitting web browsing",
             "sitting writing",
             "standing",
             "standing web browsing",
             "standing writing",
             "walking at 1mph arms on desk",
             "walking at 2mph arms on desk",
             "walking at 3mph",
             "walking at 3mph carrying drink",
             "walking at 3mph carrying bag",
             "walking at 3mph phone talking",
             "walking at 3.5mph",
             "running at 5.5mph 5% grade",
             "walking outdoor",
             "walking upstairs",
             "walking donwstairs",
             "biking at 300 KPM/Min",
             "biking outdoor",
             "sweeping",
             "laundry",
             "shelf unload",
             "shelf reload",
             "frisbee")

act_list = c("Lying",
             "Sitting",
             "Standing",
             "Walk 1 mph",
             "Walk 2 mph",
             "Laundry",
             "Frisbee",
             "Walk 3 mph",
             "Self-paced walk",
             "Bike- 300 kmph/min",
             "Sweeping",
             "Walk 3.5 mph",
             "Downstairs",
             "Reload/unload shelf",
             "Upstairs",
             "Bike outdoor",
             "Run- 5.5 mph")

mets = c(
  1,
  1.3,
  1.3,
  2,
  2.8,
  2,
  3,
  3.5,
  4,
  3.5,
  3.3,
  4.3,
  3.5,
  3.5,
  5,
  7.5,
  9
)

mets = data.frame(activity = act_list, mets_value = mets)
sorted_mets = mets[order(mets[,"mets_value"]),]

p1Data1 = spades_lab_counts

# exclude
p1Data = dplyr::filter(spades_lab_counts, !(LABEL_NAME == "Frisbee" & PID == 26))
p1Data = melt(p1Data, id.vars=c('HEADER_TIME_STAMP', 'LABEL_NAME', 'PID', 'LOCATION'), variable.name = "TYPE")

# scale to 1 min
p1Data[,"value"] = p1Data[, "value"] * acc_factor
p1Data$LABEL_NAME <- factor(p1Data$LABEL_NAME, levels=act_list)

p3Data = p1Data
p3Data$ALGORITHM = as.character(p3Data$TYPE)
p3Data$ALGORITHM[str_detect(p3Data$TYPE, "SMARTcounts")] = "MIMS-unit"
p3Data$ALGORITHM[str_detect(p3Data$TYPE, "ENMO")] = "ENMO"
p3Data$DEVICE = "8g Device"
p3Data[str_detect(p3Data$TYPE, "2g"), "DEVICE"] = "2g Device"
p3Data$DEVICE = factor(p3Data$DEVICE, c("8g Device", "2g Device"))


t_test_results = p3Data %>% ddply(.(LABEL_NAME, LOCATION), function(rows){
  print(rows$LABEL_NAME[1])
  mims_8g = rows %>% filter(ALGORITHM == "MIMS-unit" & DEVICE == "8g Device")
  mims_2g = rows %>% filter(ALGORITHM == "MIMS-unit" & DEVICE == "2g Device")
  actigraph_8g = rows %>% filter(ALGORITHM == "ACTIGRAPH_COUNT" & DEVICE == "8g Device")
  actigraph_2g = rows %>% filter(ALGORITHM == "ACTIGRAPH_COUNT" & DEVICE == "2g Device")
  enmo_8g = rows %>% filter(ALGORITHM == "ENMO" & DEVICE == "8g Device")
  enmo_2g = rows %>% filter(ALGORITHM == "ENMO" & DEVICE == "2g Device")
  mims_t = t.test(mims_8g$value, mims_2g$value, paired = FALSE)
  mims_p = mims_t$p.value
  # actigraph_t = t.test(actigraph_8g$value, actigraph_2g$value, paired = FALSE)
  # actigraph_p = actigraph_t$p.value
  enmo_t = t.test(enmo_8g$value, enmo_2g$value, paired = FALSE)
  enmo_p = enmo_t$p.value
  return(data.frame(mims_p, enmo_p))
})

current_run = format(Sys.time(), "%Y%m%d%H")

write.csv(x = t_test_results, file = paste0("reproduce/table/spades_lab_count_t_test", current_run,".csv"), quote = FALSE, row.names = FALSE, col.names = TRUE)
