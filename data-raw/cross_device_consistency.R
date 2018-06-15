
df = data.frame(readr::read_csv("D:/data/mims_shaker/data/DerivedCrossParticipants/counts.feature.csv"))

saveRDS(df, file='inst/extdata/cross_device_consistency.rds')