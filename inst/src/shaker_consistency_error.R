rm(list = ls(all.names = TRUE))
require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(ggthemes)
require(Counts)

filename1 = "inst/extdata/shaker2.rds"
filename2 = "inst/extdata/shaker3.rds"
filename3 = "inst/extdata/shaker4.rds"
shaker2 = readRDS(filename1)
shaker3 = readRDS(filename2)
shaker4 = readRDS(filename3)

shaker_raw = rbind(shaker2, shaker3, shaker4)

settings = data.frame(
  low_bandwidth = c(0.2, 0.2, 0.25, 0.25),
  high_bandwidth = c(5, 5, 2.5, 2.5),
  use_extrapolate = c(TRUE, FALSE, TRUE, FALSE),
  use_interpolate = c(TRUE, TRUE, TRUE, TRUE),
  name = c(
    "Proposed",
    "Proposed without extrapolation",
    "Proposed with narrower passband (0.25-2.5Hz)",
    "Proposed with narrower passband (0.25-2.5Hz) and without extrapolation"
  )
)

k = 0.05
spar = 0.6
noise_level = 0.03

error_data = adply(settings, 1, function(setting) {
  counts = experiment_shaker_count(
    shaker_raw,
    epoch = '5 sec',
    noise_level = noise_level,
    cutoffs = c(setting$low_bandwidth, setting$high_bandwidth),
    k = k,
    spar = spar,
    integration = 'trapz',
    use_extrapolate = setting$use_extrapolate,
    use_interpolate = setting$use_interpolate,
    use_filtering = TRUE,
    axes = c(2, 3, 4)
  )

  gt = counts %>% dplyr::filter(DEVICE == "TAS1C32140067") %>% ddply(c("RPM"),
                                                                     summarise,
                                                                     MEAN = mean(COUNT),
                                                                     VAR = var(COUNT))

  error = counts %>% ddply(c("RPM"), function(counts) {
    rpm = unique(counts$RPM)
    gt_value = gt[gt$RPM == rpm, "MEAN"]
    n_total = nrow(counts)
    cv_counts = counts %>% dplyr::filter(DEVICE != "ActivPal3")
    mse_counts = counts %>% dplyr::filter(DEVICE != "TAS1C32140067" &
                                            DEVICE != "ActivPal3")
    n_mse = nrow(mse_counts)
    rmse = sqrt(sum((mse_counts$COUNT - gt_value) ^ 2) / n_mse)
    cv_relative = rmse / gt_value

    cv = sd(cv_counts$COUNT) / mean(cv_counts$COUNT)
    return(data.frame(
      RMSE = rmse,
      CV = cv,
      CV_gt = cv_relative
    ))
  })
  return(error)
}, .progress = progress_text(), .id = NULL)

error_data = error_data[c(5, 6, 8, 9)] # don't use RMSE

# BIOBANK
filename1 = "inst/extdata/shaker2_biobank_enmo.rds"
filename2 = "inst/extdata/shaker3_biobank_enmo.rds"
filename3 = "inst/extdata/shaker4_biobank_enmo.rds"
shaker2_biobank = readRDS(filename1)
shaker3_biobank = readRDS(filename2)
shaker4_biobank = readRDS(filename3)

shaker_biobank = rbind(shaker2_biobank, shaker3_biobank, shaker4_biobank)
gt_biobank = shaker_biobank %>% dplyr::filter(DEVICE == "TAS1C32140067") %>% ddply(c("RPM"),
                                                                                                   summarise,
                                                                                                   MEAN = mean(biobank_enmo),
                                                                                                   VAR = var(biobank_enmo))
error_biobank = shaker_biobank %>% ddply(c("RPM"), function(counts) {
  rpm = unique(counts$RPM)
  gt_value = gt_biobank[gt_biobank$RPM == rpm, "MEAN"]
  n_total = nrow(counts)
  mse_counts = counts %>% dplyr::filter(DEVICE != "TAS1C32140067" &
                                          DEVICE != "ActivPal3")
  cv_counts = counts %>% dplyr::filter(DEVICE != "ActivPal3")

  n_mse = nrow(mse_counts)
  rmse = sqrt(sum((mse_counts$biobank_enmo - gt_value) ^ 2) / n_mse)
  cv_relative = rmse / gt_value
  cv = sd(cv_counts$biobank_enmo) / mean(cv_counts$biobank_enmo)
  return(data.frame(RMSE = rmse, CV = cv, CV_gt = cv_relative))
})

error_biobank$name = "UK Biobank ENMO"
error_biobank = error_biobank[c(5, 1, 3, 4)]

# Actigraph Count
filename1 = "inst/extdata/shaker2_count_actigraph.rds"
filename2 = "inst/extdata/shaker3_count_actigraph.rds"
filename3 = "inst/extdata/shaker4_count_actigraph.rds"
shaker2_actigraph = readRDS(filename1)
shaker3_actigraph = readRDS(filename2)
shaker4_actigraph = readRDS(filename3)

shaker_actigraph_count = rbind(shaker2_actigraph, shaker3_actigraph, shaker4_actigraph)
gt_actigraph_count = shaker_actigraph_count %>% dplyr::filter(DEVICE == "TAS1C32140067") %>% ddply(c("RPM"),
                                                                                                   summarise,
                                                                                                   MEAN = mean(ACTIGRAPH),
                                                                                                   VAR = var(ACTIGRAPH))
error_actigraph = shaker_actigraph_count %>% ddply(c("RPM"), function(counts) {
  rpm = unique(counts$RPM)
  gt_value = gt_actigraph_count[gt_actigraph_count$RPM == rpm, "MEAN"]
  n_total = nrow(counts)
  mse_counts = counts %>% dplyr::filter(DEVICE != "TAS1C32140067" &
                                          DEVICE != "ActivPal3")
  cv_counts = counts %>% dplyr::filter(DEVICE != "ActivPal3")

  n_mse = nrow(mse_counts)
  rmse = sqrt(sum((mse_counts$ACTIGRAPH - gt_value) ^ 2) / n_mse)
  cv_relative = rmse / gt_value
  cv = sd(cv_counts$ACTIGRAPH) / mean(cv_counts$ACTIGRAPH)
  return(data.frame(RMSE = rmse, CV = cv, CV_gt = cv_relative))
})

error_actigraph$name = "Actigraph count algorithm"
error_actigraph = error_actigraph[c(5, 1, 3, 4)]


error_data$name = as.character(error_data$name)
error_data_merged = rbind(error_data, error_actigraph, error_biobank)
error_data_merged = error_data_merged[c(1, 2, 3)]
error_data_melted = error_data_merged %>% dplyr::filter(
  !name == "Proposed with narrower passband (0.25-2.5Hz) and without extrapolation, resample to 10Hz"
) %>% melt(c("RPM", "name"))
current_run = format(Sys.time(), "%Y%m%d%H")
write.csv(error_data_melted, file = paste0("inst/table/shaker_consistency_cv", current_run ,".csv"), quote = FALSE, row.names = FALSE)

p = ggplot(error_data_melted,
           aes(
             x = RPM,
             y = value,
             linetype = name,
             shape = name
           )) +

  geom_line() +
  geom_point(size = 3) +

  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.margin = unit(10 ^ -3, 'inch'),
    panel.margin = unit(0.1, 'inch'),
    axis.text = element_text(margin = margin(0, 0, 0, 0)),
    plot.margin = margin(0, 0, 0, 0, 'inch'),
    strip.background = element_blank(),
    legend.key = element_blank()
  ) +
  # facet_wrap(~ variable, scales = "free_y") +
  guides(shape = guide_legend(ncol = 2, title = NULL),
         linetype = guide_legend(ncol = 2, title = NULL)) +
  ylab("Coefficient of variance (CV)") +
  xlab("Frequency (Hz)")

ggsave(
  filename = paste0("shaker_consistency_error_",current_run,".png"),
  plot = p,
  path = "inst/figure/",
  scale = 2,
  width = 4,
  height = 2
)
