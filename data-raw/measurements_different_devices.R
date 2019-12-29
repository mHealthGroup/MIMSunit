## code to prepare `measurements_different_devices` dataset goes here
library(magrittr)
library(plyr)
library(dplyr)

# load preprocessed dataset from the other dataset repository
input_data <- data.frame(
  readr::read_csv(
    "../MIMSunit-dataset-shaker/data/DerivedCrossParticipants/counts.feature.csv"
  )
)

acc_factor <- 60 / 5
measurements_different_devices <- input_data %>% ddply(
  c("DEVICE", "GRANGE", "SR", "TYPE", "HZ", "NAME"),
  summarise,
  mean = mean(.data$VALUE * acc_factor),
  sd   = sd(.data$VALUE * acc_factor),
)


usethis::use_data(measurements_different_devices)
