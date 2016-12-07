require(reshape2)
require(plyr)
require(dplyr)
require(ggplot2)
require(mHealthR)

# extrapolation parameters ----
k = 0.05
spar = 0.6
confidence = 0.5
noise_level = 0.03

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

colors = gg_color_hue(2)

# examples ----

example_shaker = function(){
filename = "inst/extdata/shaker2.rds"
shaker2 = readRDS(filename)

g3Shaker = shaker2 %>% dplyr::filter(GRANGE == 3 &
                                       RPM == 5) %>% select(c(1, 2))
g3Shaker = g3Shaker %>% mHealthR::mhealth.clip(start_time = g3Shaker[1, 1] + 40, stop_time = g3Shaker[1, 1] + 50, file_type = "sensor")

g6Shaker = shaker2 %>% dplyr::filter(GRANGE == 6 &
                                       RPM == 5) %>% select(c(1, 2))
g6Shaker = g6Shaker %>% mHealthR::mhealth.clip(start_time = g6Shaker[1, 1] + 40, stop_time = g6Shaker[1, 1] + 50, file_type = "sensor")


g6Shaker[[mhealth$column$TIMESTAMP]] = g6Shaker[[mhealth$column$TIMESTAMP]] - 0.11

g3Shaker_extrap = Counts::extrapolate.data.frame(
  g3Shaker,
  range = c(-3, 3),
  noise_level = noise_level,
  k = k,
  spar = spar
)

forPlot = rbind(
  cbind(g6Shaker, group = "gt"),
  cbind(g3Shaker_extrap, group = "extrap"),
  cbind(g3Shaker, group = "origin")
)

labels = c("GT3XBT (80Hz, 6g)", "Extrapolated GT3X", "GT3X (30Hz, 3g)")

p1 = ggplot(
  data = forPlot,
  aes(
    x = HEADER_TIME_STAMP,
    y = X_ACCELATION_METERS_PER_SECOND_SQUARED,
    colour = group,
    lty = group,
    alpha = group
  )
) +
  geom_line(lwd = 1) +
  theme_minimal(base_size = 16) +
  scale_color_manual(values = c("gray", colors[2], colors[1]),
                     labels = labels) +
  scale_linetype_manual(values = c("solid", "solid", "solid"),
                        labels = labels) +
  scale_alpha_manual(values = c(1, 0.4, 1), labels = labels) +
  guides(
    colour = guide_legend(title = ""),
    linetype = guide_legend(title = ""),
    alpha = guide_legend(title = "")
  ) +
  xlab("time (s)") +
  ylab("X-axis acceleration (g)") +
  theme(legend.position = "bottom")

ggsave(
  path = file.path("inst/figure/"),
  filename = "extrapolate_example_shaker.png",
  plot = p1,
  scale = 2,
  width = 4,
  height = 2
)
}


example_running1 = function(){
filename = "inst/extdata/running_maxed_out.rds"
running_maxed_out = readRDS(filename)

maxed_out_running = running_maxed_out %>% dplyr::filter(TYPE == "GT3X")
gt_running = running_maxed_out %>% dplyr::filter(TYPE == "GT9X")
grange = unique(maxed_out_running$GRANGE)
extrapolatedData = Counts::extrapolate.data.frame(
  maxed_out_running[, 1:4],
  range = c(-grange, grange),
  noise_level = noise_level,
  k = k,
  spar = spar
)

extrapolated_clip = extrapolatedData[, 1:2] %>% mHealthR::mhealth.clip(start_time = extrapolatedData[1, 1] + 20, stop_time = extrapolatedData[1, 1] + 30, file_type = "sensor")
maxedout_clip = maxed_out_running[, 1:2] %>% mHealthR::mhealth.clip(start_time = extrapolatedData[1, 1] + 20, stop_time = extrapolatedData[1, 1] + 30, file_type = "sensor")
gt_clip = gt_running[, 1:2] %>% mHealthR::mhealth.clip(start_time = extrapolatedData[1, 1] + 20, stop_time = extrapolatedData[1, 1] +  30, file_type = "sensor")

# plot
#
forPlot = rbind(
  cbind(gt_clip, group = "gt"),
  cbind(extrapolated_clip, group = "extrap"),
  cbind(maxedout_clip, group = "origin")
)

labels = c("GT9X (80Hz, 8g)", "Extrapolated GT3X", "GT3X (30Hz, 3g)")
p2 = ggplot(data = forPlot,
            aes(
              x = HEADER_TIME_STAMP,
              y = RAWX,
              colour = group,
              lty = group,
              alpha = group
            )) +
  geom_line(lwd = 1) +
  theme_minimal(base_size = 16) +
  scale_color_manual(values = c("gray", colors[2], colors[1]),
                     labels = labels) +
  scale_linetype_manual(values = c("solid", "solid", "solid"),
                        labels = labels) +
  scale_alpha_manual(values = c(1, 0.4, 1), labels = labels) +
  guides(
    colour = guide_legend(title = ""),
    linetype = guide_legend(title = ""),
    alpha = guide_legend(title = "")
  ) +
  xlab("time (s)") +
  ylab("X-axis acceleration (g)") +
  theme(legend.position = "bottom")

ggsave(
  path = file.path("inst/figure/"),
  filename = "extrapolate_example_running_1.png",
  plot = p2,
  scale = 2,
  width = 4,
  height = 2
)
}

example_jumpingjack = function(){
filename = "inst/extdata/jumping_jack_maxed_out.rds"
jumping_jack_maxed_out = readRDS(filename)

maxed_out_jj = jumping_jack_maxed_out %>% dplyr::filter(GRANGE == 3)
gt_jj = jumping_jack_maxed_out %>% dplyr::filter(GRANGE == 8)
extrap_jj = extrapolate.data.frame(
  maxed_out_jj[, 1:2],
  range = c(-3, 3),
  noise_level = noise_level,
  k = k,
  spar = spar
)

# plot
#
forPlot = rbind(
  cbind(gt_jj[, 1:2], group = "gt"),
  cbind(extrap_jj, group = "extrap"),
  cbind(maxed_out_jj[, 1:2], group = "origin")
)

labels = c("8g device", "extrapolated 3g signal", "8g device cropped to 3g")
p3 = ggplot(
  data = forPlot,
  aes(
    x = HEADER_TIME_STAMP,
    y = X_ACCELERATION_METERS_PER_SECOND_SQUARED,
    colour = group,
    lty = group,
    alpha = group
  )
) +
  geom_line(lwd = 1) +
  theme_minimal(base_size = 16) +
  scale_color_manual(values = c("gray", colors[2], colors[1]),
                     labels = labels) +
  scale_linetype_manual(values = c("solid", "solid", "solid"),
                        labels = labels) +
  scale_alpha_manual(values = c(1, 0.4, 1), labels = labels) +
  guides(
    colour = guide_legend(title = ""),
    linetype = guide_legend(title = ""),
    alpha = guide_legend(title = "")
  ) +
  xlab("time (s)") +
  ylab("X-axis acceleration (g)") +
  theme(legend.position = "bottom")

ggsave(
  path = file.path("inst/figure/"),
  filename = "extrapolate_example_jumpingjack.png",
  plot = p3,
  scale = 2,
  width = 4,
  height = 2
)
}

example_frisbee = function(){
filename = "inst/extdata/frisbee_maxed_out.rds"
frisbee_maxed_out = readRDS(filename)

maxed_out_fb = frisbee_maxed_out %>% dplyr::filter(GRANGE == 3)
gt_fb = frisbee_maxed_out %>% dplyr::filter(GRANGE == 8)

# clip
st = maxed_out_fb[1, 1] + 22
maxed_out_fb = maxed_out_fb %>% mHealthR::mhealth.clip(start_time = st, stop_time = st + 10, file_type = "sensor")
gt_fb = gt_fb %>% mHealthR::mhealth.clip(start_time = st, stop_time = st + 10, file_type = "sensor")

extrap_fb = extrapolate.data.frame(
  maxed_out_fb[, 1:2],
  range = c(-3, 3),
  noise_level = noise_level,
  k = k,
  spar = spar
)

# plot
#
forPlot = rbind(
  cbind(gt_fb[, 1:2], group = "gt"),
  cbind(extrap_fb, group = "extrap"),
  cbind(maxed_out_fb[, 1:2], group = "origin")
)

labels = c("8g device", "extrapolated 3g signal", "8g device cropped to 3g")
p4 = ggplot(
  data = forPlot,
  aes(
    x = HEADER_TIME_STAMP,
    y = X_ACCELERATION_METERS_PER_SECOND_SQUARED,
    colour = group,
    lty = group,
    alpha = group
  )
) +
  geom_line(lwd = 1) +
  theme_minimal(base_size = 16) +
  scale_color_manual(values = c("gray", colors[2], colors[1]),
                     labels = labels) +
  scale_linetype_manual(values = c("solid", "solid", "solid"),
                        labels = labels) +
  scale_alpha_manual(values = c(1, 0.4, 1), labels = labels) +
  guides(
    colour = guide_legend(title = ""),
    linetype = guide_legend(title = ""),
    alpha = guide_legend(title = "")
  ) +
  xlab("time (s)") +
  ylab("X-axis acceleration (g)") +
  theme(legend.position = "bottom")

ggsave(
  path = file.path("inst/figure/"),
  filename = "extrapolate_example_frisbee.png",
  plot = p4,
  scale = 2,
  width = 4,
  height = 2
)
}

example_running2 = function(){
filename = "inst/extdata/running2_maxed_out.rds"
running2_maxed_out = readRDS(filename)

maxed_out_r2 = running2_maxed_out %>% dplyr::filter(GRANGE == 3)
gt_r2 = running2_maxed_out %>% dplyr::filter(GRANGE == 8)

# clip
st = maxed_out_r2[1, 1]
maxed_out_r2 = maxed_out_r2 %>% mHealthR::mhealth.clip(start_time = st, stop_time = st + 10, file_type = "sensor")
gt_r2 = gt_r2 %>% mHealthR::mhealth.clip(start_time = st, stop_time = st + 10, file_type = "sensor")

extrap_r2 = extrapolate.data.frame(
  maxed_out_r2[, 1:4],
  range = c(-3, 3),
  noise_level = noise_level,
  k = k,
  spar = spar
)

# plot
#
forPlot = rbind(
  cbind(gt_r2[, c(1, 3)], group = "gt"),
  cbind(extrap_r2[, c(1, 3)], group = "extrap"),
  cbind(maxed_out_r2[, c(1, 3)], group = "origin")
)

labels = c("8g device", "extrapolated 3g signal", "8g device cropped to 3g")
p5 = ggplot(
  data = forPlot,
  aes(
    x = HEADER_TIME_STAMP,
    y = Y_ACCELERATION_METERS_PER_SECOND_SQUARED,
    colour = group,
    lty = group,
    alpha = group
  )
) +
  geom_line(lwd = 1) +
  theme_minimal(base_size = 16) +
  scale_color_manual(values = c("gray", colors[2], colors[1]),
                     labels = labels) +
  scale_linetype_manual(values = c("solid", "solid", "solid"),
                        labels = labels) +
  scale_alpha_manual(values = c(1, 0.4, 1), labels = labels) +
  guides(
    colour = guide_legend(title = ""),
    linetype = guide_legend(title = ""),
    alpha = guide_legend(title = "")
  ) +
  xlab("time (s)") +
  ylab("Y-axis acceleration (g)") +
  theme(legend.position = "bottom")

ggsave(
  path = file.path("inst/figure/"),
  filename = "extrapolate_example_running_2.png",
  plot = p5,
  scale = 2,
  width = 4,
  height = 2
)
}

example_running3 = function(){
filename = "inst/extdata/treadmill1.rds"
walkrun1 = readRDS(filename)

gt_running = walkrun1 %>% dplyr::filter(
  SR == "40" &
    MPH == 8 &
    WEIGHTS == "0" &
    LOCATION == "NondominantWrist"
)
maxed_out_running = gt_running %>% Counts::crop_grange(range = c(-3, 3), noise_std = noise_level * 1.5)
extrap_running = extrapolate.data.frame(
  maxed_out_running[, 1:4],
  range = c(-3, 3),
  noise_level = noise_level,
  k = k,
  spar = spar
)
startTime = extrap_running[1, 1]
extrap_clip = extrap_running[, c(1, 3)] %>% mHealthR::mhealth.clip(start_time = startTime, stop_time = startTime + 10, file_type = "sensor")
maxedout_clip = maxed_out_running[, c(1, 3)] %>% mHealthR::mhealth.clip(start_time = startTime, stop_time = startTime + 10, file_type = "sensor")
gt_clip = gt_running[, c(1, 3)] %>% mHealthR::mhealth.clip(start_time = startTime, stop_time = startTime + 10, file_type = "sensor")

# plot
#
forPlot = rbind(
  cbind(gt_clip, group = "gt"),
  cbind(extrap_clip, group = "extrap"),
  cbind(maxedout_clip, group = "origin")
)

labels = c("GT9X (40Hz, 8g)", "Extrapolated GT9X", "GT9X (40Hz, 3g)")
p2 = ggplot(
  data = forPlot,
  aes(
    x = HEADER_TIME_STAMP,
    y = Y,
    colour = group,
    lty = group,
    alpha = group
  )
) +
  geom_line(lwd = 1) +
  theme_minimal(base_size = 16) +
  scale_color_manual(values = c("gray", colors[2], colors[1]),
                     labels = labels) +
  scale_linetype_manual(values = c("solid", "solid", "solid"),
                        labels = labels) +
  scale_alpha_manual(values = c(1, 0.4, 1), labels = labels) +
  guides(
    colour = guide_legend(title = ""),
    linetype = guide_legend(title = ""),
    alpha = guide_legend(title = "")
  ) +
  xlab("time (s)") +
  ylab("Y-axis acceleration (g)") +
  theme(legend.position = "bottom")

ggsave(
  path = file.path("inst/figure/"),
  filename = "extrapolate_example_running_3.png",
  plot = p2,
  scale = 2,
  width = 4,
  height = 2
)
}

example_running4 = function(){
  filename = "inst/extdata/treadmill1.rds"
  walkrun1 = readRDS(filename)

  gt_running = walkrun1 %>% dplyr::filter(
    SR == "100" &
      MPH == 8 &
      WEIGHTS == "0" &
      SUBJECT == "P1" & SESSION == "1" & LOCATION == "NondominantWrist"
  )
  maxed_out_running = gt_running %>% Counts::crop_grange(range = c(-3, 3), noise_std = noise_level * 1.5)
  extrap_running = Counts::extrapolate.data.frame(
    maxed_out_running[, 1:4],
    range = c(-3, 3),
    noise_level = noise_level,
    k = k,
    spar = spar
  )
  startTime = extrap_running[1, 1]
  extrap_clip = extrap_running[, c(1, 3)] %>% mHealthR::mhealth.clip(start_time = startTime, stop_time = startTime + 10, file_type = "sensor")
  maxedout_clip = maxed_out_running[, c(1, 3)] %>% mHealthR::mhealth.clip(start_time = startTime, stop_time = startTime + 10, file_type = "sensor")
  gt_clip = gt_running[, c(1, 3)] %>% mHealthR::mhealth.clip(start_time = startTime, stop_time = startTime + 10, file_type = "sensor")

  # plot
  #
  forPlot = rbind(
    cbind(gt_clip, group = "gt"),
    cbind(extrap_clip, group = "extrap"),
    cbind(maxedout_clip, group = "origin")
  )

  labels = c("GT9X (100Hz, 8g)", "Extrapolated GT9X", "GT9X (100Hz, 3g)")
  p2 = ggplot(
    data = forPlot,
    aes(
      x = HEADER_TIME_STAMP,
      y = Y,
      colour = group,
      lty = group,
      alpha = group
    )
  ) +
    geom_line(lwd = 1) +
    theme_minimal(base_size = 16) +
    scale_color_manual(values = c("gray", colors[2], colors[1]),
                       labels = labels) +
    scale_linetype_manual(values = c("solid", "solid", "solid"),
                          labels = labels) +
    scale_alpha_manual(values = c(1, 0.4, 1), labels = labels) +
    guides(
      colour = guide_legend(title = ""),
      linetype = guide_legend(title = ""),
      alpha = guide_legend(title = "")
    ) +
    xlab("time (s)") +
    ylab("Y-axis acceleration (g)") +
    theme(legend.position = "bottom")

  ggsave(
    path = file.path("inst/figure/"),
    filename = "extrapolate_example_running_4.png",
    plot = p2,
    scale = 2,
    width = 4,
    height = 2
  )
}

example_shaker()
example_jumpingjack()
example_frisbee()
example_running1()
example_running2()
example_running3()
example_running4()
