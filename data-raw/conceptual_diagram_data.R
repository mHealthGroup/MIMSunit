## code to prepare `conceptual_diagram` dataset goes here

require(plyr)
require(dplyr)

k <- 0.05
spar <- 0.6
confidence <- 0.5
noise_level <- 0.03

filename <- "reproduce/extdata/jumping_jack_maxed_out.rds"
jumping_jack_maxed_out <- readRDS(filename)

# device 0: 80Hz, 8g
device0 <- jumping_jack_maxed_out %>% filter(GRANGE == 8)
device0 <- device0[, 1:2]

# # add some impulse
spike_values <- -c(
  -0.023,
  -0.789,
  -2.488,
  -3.854,
  -2.532 - 1.883,
  -0.879,
  -0.566,
  -0.136,
  0,
  0.24,
  1.2
)
spike_x <- seq(0, 0.12, 0.01)
spike_x_80 <- seq(0, 0.12, 1 / 80)
spike_values_80 <- signal::interp1(spike_x, spike_values, spike_x_80,
  method =
    "linear"
)
spike_values_80[9:10] <- c(-0.823, -1.234)
begin_value <- device0[16, 2]
device0[16:25, 2] <- spike_values_80
device0["SR"] <- 80
device0["GRANGE"] <- 8
device0["NAME"] <- "Device 0 (80Hz, 8g)"

# device 1: 40Hz, 2g
device1 <- MIMSunit::simulate_new_data(device0[, c(1, 2)], c(-2, 2), 40)
device1["SR"] <- 40
device1["GRANGE"] <- 2
device1["NAME"] <- "Device 1 (40Hz, 2g)"

# device 2: 30Hz, 4g
device2 <- MIMSunit::simulate_new_data(device0[, c(1, 2)], c(-4, 4), 30)
device2["SR"] <- 30
device2["GRANGE"] <- 4
device2["NAME"] <- "Device 2 (30Hz, 4g)"

# device 3: 20Hz, 2g
device3 <- MIMSunit::simulate_new_data(device0[, c(1, 2)], c(-2, 2), 20)
device3["SR"] <- 20
device3["GRANGE"] <- 2
device3["NAME"] <- "Device 3 (20Hz, 2g)"

start_time <- device0[[1, 1]]
stop_time <- start_time + 1 # one second

conceptual_diagram_data <- rbind(device0, device1, device2, device3)
colnames(conceptual_diagram_data) <- c("HEADER_TIME_STAMP", "X", "SR", "GRANGE", "NAME")

usethis::use_data(conceptual_diagram_data, overwrite = TRUE)
