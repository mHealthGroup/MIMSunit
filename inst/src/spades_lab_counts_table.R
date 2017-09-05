filename = "inst/extdata/spades_lab_counts.rds"
spades_lab_counts = readRDS(filename)

# optimize scaling
require(dplyr)
require(stringr)
require(ggplot2)
require(reshape2)
require(Counts)
forScaling = dplyr::filter(spades_lab_counts, ACTIVITY_NAME %in% c("walking at 1mph arms on desk",
                                                                   "walking at 2mph arms on desk",
                                                                   "walking at 3mph",
                                                                   "walking at 3mph carrying drink",
                                                                   "walking at 3mph carrying bag",
                                                                   "walking at 3mph phone talking",
                                                                   "walking at 3.5mph") & LOCATION == "DOMINANT WAIST")

forScaling_counts = forScaling %>% dplyr::filter(TYPE == "COUNTS")
forScaling_actigraph = forScaling %>% dplyr::filter(TYPE == "ACTIGRAPH")
countsForScaling = data.frame(count = forScaling_counts$value, actigraph = forScaling_actigraph$value)

regressionResult = lm(formula = actigraph ~ count, data = countsForScaling, na.action = na.omit)
scalingFactor = coef(regressionResult)

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

act_list = c("lying",
             "sitting",
             "standing",
             "walking at 1mph arms on desk",
             "walking at 2mph arms on desk",
             "walking at 3mph",
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

mets = c(
  1.3,
  1.5,
  1.8,
  2,
  2.8,
  3.5,
  4.3,
  9.8,
  3.5,
  6,
  5,
  3.5,
  6.8,
  3.8,
  2,
  5,
  5,
  3
)

mets = data.frame(activity = act_list, mets_value = mets)
sorted_mets = mets[order(mets[,"mets_value"]),]

p1Data1 = spades_lab_counts

# merge similar activities
p1Data1[str_detect(p1Data1[,1], "sitting"),1] = "sitting"
p1Data1[str_detect(p1Data1[,1], "standing"),1] = "standing"
p1Data1[str_detect(p1Data1[,1], "walking at 3mph"),1] = "walking at 3mph"

# exclude
p1Data = dplyr::filter(p1Data1, !(ACTIVITY_NAME == "frisbee" & SUBJECT == "SPADES_26"))

count_indices = which(p1Data$TYPE == "COUNTS" | p1Data$TYPE == "COUNTS_MAXEDOUT")
p1Data[count_indices, "value"] = p1Data[count_indices, "value"] * scalingFactor[2] + scalingFactor[1]
p1Data$ACTIVITY_NAME <- factor(p1Data$ACTIVITY_NAME, levels=sorted_mets[,1])
p1Data$SUBJECT_ID <- as.numeric(str_extract(p1Data$SUBJECT, "[0-9]+"))
p1Data = p1Data %>% mutate(LOCATION_TYPE = str_sub(LOCATION, start = -5, end = -1)) %>% mutate(LOCATION_SIDE = str_sub(LOCATION, start = 1, end = -7))

# summarize
# p1Data_summary <- ddply(p1Data, c("ACTIVITY_NAME", "START_TIME", "STOP_TIME", "LABEL_NAME", "TYPE", 'LOCATION', 'LOCATION_TYPE', 'LOCATION_SIDE'), summarise,
#                N    = length(value),
#                mean = mean(value),
#                sd   = sd(value),
#                se   = sd / sqrt(N)
# )

# scatter + box plot
p1Data = p1Data %>% dplyr::filter(LOCATION == "NON DOMINANT WRIST" | LOCATION == "DOMINANT WAIST")
p1Data$LOCATION = str_to_title(p1Data$LOCATION)

t_test_data = p1Data %>% ddply(.(ACTIVITY_NAME, LOCATION), function(rows){
  counts = rows %>% filter(TYPE == "COUNTS")
  counts_maxedout = rows %>% filter(TYPE == "COUNTS_MAXEDOUT")
  actigraph = rows %>% filter(TYPE == "ACTIGRAPH")
  actigraph_maxedout = rows %>% filter(TYPE == "ACTIGRAPH_MAXEDOUT")
  counts_t = t.test(counts$value, counts_maxedout$value, paired = FALSE)
  counts_p = counts_t$p.value
  actigraph_t = t.test(actigraph$value, actigraph_maxedout$value, paired = FALSE)
  actigraph_p = actigraph_t$p.value
  return(data.frame(counts_p, actigraph_p))
})

write.csv(x = t_test_data, file = "inst/table/spades_lab_count_t_test.csv", quote = FALSE, row.names = FALSE, col.names = TRUE)
