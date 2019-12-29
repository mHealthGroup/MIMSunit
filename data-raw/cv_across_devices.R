## code to prepare `cv_across_devices` dataset goes here
library(magrittr)
library(plyr)
library(dplyr)

# load preprocessed dataset from the other dataset repository
input_data <- data.frame(
  readr::read_csv(
    "../MIMSunit-dataset-shaker/data/DerivedCrossParticipants/counts.feature.csv"
  )
)

cv_different_algorithms <- input_data %>%
  filter(.data$DEVICE != "Activpal") %>% # Do not count ActivPal as it does not work with Actigraph counts
  ddply(
    c("TYPE", "HZ"),
    summarise,
    COEFF_OF_VARIATION = sd(.data$VALUE) / mean(.data$VALUE)
  )

usethis::use_data(cv_different_algorithms, overwrite = TRUE)
