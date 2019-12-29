require(reshape2)
require(plyr)
require(dplyr)
require(ggplot2)
require(Counts)

# extrapolation parameters ----
k = 0.05
spar = 0.6
noise_level = 0.03

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

colors = gg_color_hue(2)

# examples ----

example_shaker_3g = function(){
  filename = "reproduce/extdata/shaker2.rds"
  shaker2 = readRDS(filename)
  g3Shaker = shaker2 %>% dplyr::filter(GRANGE == 3 & RPM == 5) %>% select(c(1, 2))
  g3Shaker = g3Shaker %>% MIMSunit::clip_data(start_time = g3Shaker[1, 1] + 74, stop_time = g3Shaker[1, 1] + 82)
  g6Shaker = shaker2 %>% dplyr::filter(GRANGE == 6 & RPM == 5) %>% select(c(1, 2))
  g6Shaker = g6Shaker %>% MIMSunit::clip_data(start_time = g6Shaker[1, 1] + 74, stop_time = g6Shaker[1, 1] + 82)
  g6Shaker[[1]] = g6Shaker[[1]] + 0.06
  g3Shaker_extrap = extrapolate(g3Shaker, range = c(-3, 3), noise_level = noise_level, k = k, spar = spar)
  forPlot = rbind(cbind(g6Shaker, group = "gt"),
                cbind(g3Shaker_extrap, group = "extrap"),
                cbind(g3Shaker, group = "origin")
                )
  write.csv(x = forPlot, file = "reproduce/table/extrapolation_examples_shaker_3g.csv", append = F, quote = FALSE, row.names = FALSE, col.names = TRUE)
  labels = c("GT3XBT (80Hz, 6g)", "Extrapolated GT3X", "GT3X (30Hz, 3g)")
  p1 = ggplot(data = forPlot, aes(
                      x = HEADER_TIME_STAMP,
                      y = X_ACCELATION_METERS_PER_SECOND_SQUARED,
                      colour = group,
                      lty = group,
                      alpha = group
                      )
                    ) +
                    geom_line(lwd = 1) +
                    theme_minimal(base_size = 16, base_family = "Times New Roman") +
                    scale_color_manual(values = c("gray", 'black', 'black'),labels = labels) +
                    scale_linetype_manual(values = c("solid", "solid", "solid"),labels = labels) +
                    scale_alpha_manual(values = c(0.8, 0.4, 1), labels = labels) +
                    guides(
                      colour = guide_legend(title = ""),
                      linetype = guide_legend(title = ""),
                      alpha = guide_legend(title = "")
                    ) +
                    xlab("time (s)") +
                    ylab("X-axis acceleration (g)") +
                    theme(legend.position = "bottom")
  ggsave(path = file.path("reproduce/figure/"),
        filename = "extrapolate_example_shaker.png",
        plot = p1,
        scale = 2,
        width = 4,
        height = 2
  )
}

example_shaker = function(){
filename = "reproduce/extdata/shaker3.rds"
shaker3 = readRDS(filename)
filename = "reproduce/extdata/shaker4.rds"
shaker4 = readRDS(filename)

g2Shaker = shaker4 %>% dplyr::filter(GRANGE == 2 &
                                       RPM == 6) %>% select(c(1, 2))
g2Shaker = g2Shaker %>% MIMSunit::clip_data(start_time = g2Shaker[1, 1] + 40, stop_time = g2Shaker[1, 1] + 50)

g8Shaker = shaker3 %>% dplyr::filter(GRANGE == 8 &
                                       RPM == 6) %>% select(c(1, 2))
g8Shaker = g8Shaker %>% MIMSunit::clip_data(start_time = g8Shaker[1, 1] + 40, stop_time = g8Shaker[1, 1] + 50)

# shift

g2Shaker_extrap = MIMSunit::extrapolate(
  g2Shaker,
  range = c(-2, 2),
  noise_level = noise_level,
  k = k,
  spar = spar
)

g2Shaker_extrap = MIMSunit::clip_data(g2Shaker_extrap, start_time = g2Shaker_extrap[1,1] + 0.5, stop_time = g2Shaker_extrap[nrow(g2Shaker_extrap),1] - 0.5)

g2Shaker = MIMSunit::clip_data(g2Shaker, start_time = g2Shaker[1,1] + 0.5, stop_time = g2Shaker[nrow(g2Shaker),1] - 0.5)

g8Shaker = MIMSunit::clip_data(g8Shaker, start_time = g8Shaker[1,1] + 0.5, stop_time = g8Shaker[nrow(g8Shaker),1] - 0.5)

g2Shaker[[1]] = g2Shaker[[1]] - g2Shaker[1,1]
g2Shaker_extrap[[1]] = g2Shaker_extrap[[1]] - g2Shaker_extrap[1,1]
g8Shaker[[1]] = g8Shaker[[1]] - g8Shaker[1,1] - 0.05
shaker_rate = Counts::extrapolate_rate(g2Shaker, g8Shaker, g2Shaker_extrap)

forPlot = rbind(
  cbind(g8Shaker, group = "gt"),
  cbind(g2Shaker_extrap, group = "extrap"),
  cbind(g2Shaker, group = "origin")
)

write.csv(x = forPlot, file = "reproduce/table/extrapolation_examples_shaker_2g.csv", append = F, quote = FALSE, row.names = FALSE, col.names = TRUE)

labels = c("GT9X (80Hz, 8g)", "Extrapolated LG Watch", "LG Urbane R (100Hz, 2g)")

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
  theme_minimal(base_size = 16, base_family = "Times New Roman") +
  scale_color_manual(values = c("gray", 'black', 'black'),labels = labels) +
  scale_linetype_manual(values = c("solid", "solid", "solid"),labels = labels) +
  scale_alpha_manual(values = c(0.8, 0.4, 1), labels = labels) +
  guides(
    colour = guide_legend(title = ""),
    linetype = guide_legend(title = ""),
    alpha = guide_legend(title = "")
  ) +
  xlab("time (s)") +
  ylab("X-axis acceleration (g)") +
  theme(legend.position = "bottom")

ggsave(
  path = file.path("reproduce/figure/"),
  filename = "extrapolate_example_shaker_2g.png",
  plot = p1,
  scale = 2,
  width = 4,
  height = 2
)
return(shaker_rate)
}


example_running1 = function(){
filename = "reproduce/extdata/running_maxed_out.rds"
running_maxed_out = readRDS(filename)

maxed_out_running = running_maxed_out %>% dplyr::filter(TYPE == "GT3X")
gt_running = running_maxed_out %>% dplyr::filter(TYPE == "GT9X")
grange = unique(maxed_out_running$GRANGE)
extrapolatedData = MIMSunit::extrapolate(
  maxed_out_running[, 1:4],
  range = c(-grange, grange),
  noise_level = noise_level,
  k = k,
  spar = spar
)

running_rate = Counts::extrapolate_rate(maxed_out_running, gt_running, extrapolatedData)

extrapolated_clip = extrapolatedData[, 1:2] %>% MIMSunit::clip_data(start_time = extrapolatedData[1, 1] + 20, stop_time = extrapolatedData[1, 1] + 30)
maxedout_clip = maxed_out_running[, 1:2] %>% MIMSunit::clip_data(start_time = extrapolatedData[1, 1] + 20, stop_time = extrapolatedData[1, 1] + 30)
gt_clip = gt_running[, 1:2] %>% MIMSunit::clip_data(start_time = extrapolatedData[1, 1] + 20, stop_time = extrapolatedData[1, 1] +  30)

# plot
#
forPlot = rbind(
  cbind(gt_clip, group = "gt"),
  cbind(extrapolated_clip, group = "extrap"),
  cbind(maxedout_clip, group = "origin")
)

write.csv(x = forPlot, file = "reproduce/table/extrapolation_examples_running_1.csv", append = F, quote = FALSE, row.names = FALSE, col.names = TRUE)

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
  scale_color_manual(values = c("gray", 'black', 'black'),labels = labels) +
  scale_linetype_manual(values = c("solid", "solid", "solid"),labels = labels) +
  scale_alpha_manual(values = c(0.8, 0.4, 1), labels = labels) +
  guides(
    colour = guide_legend(title = ""),
    linetype = guide_legend(title = ""),
    alpha = guide_legend(title = "")
  ) +
  xlab("time (s)") +
  ylab("X-axis acceleration (g)") +
  theme(legend.position = "bottom")

ggsave(
  path = file.path("reproduce/figure/"),
  filename = "extrapolate_example_running_1.png",
  plot = p2,
  scale = 2,
  width = 4,
  height = 2
)
return(running_rate)
}

example_jumpingjack = function(){
filename = "reproduce/extdata/jumping_jack_maxed_out.rds"
jumping_jack_maxed_out = readRDS(filename)

maxed_out_jj = jumping_jack_maxed_out %>% dplyr::filter(GRANGE == 2)
gt_jj = jumping_jack_maxed_out %>% dplyr::filter(GRANGE == 8)
extrap_jj = extrapolate(
  maxed_out_jj[, 1:2],
  range = c(-2, 2),
  noise_level = noise_level,
  k = k,
  spar = spar
)

jj_rate = Counts::extrapolate_rate(maxed_out_jj, gt_jj, extrap_jj)

# plot
#
forPlot = rbind(
  cbind(gt_jj[, 1:2], group = "gt"),
  cbind(extrap_jj, group = "extrap"),
  cbind(maxed_out_jj[, 1:2], group = "origin")
)

write.csv(x = forPlot, file = "reproduce/table/extrapolation_examples_jumpingjack.csv", append = F, quote = FALSE, row.names = FALSE, col.names = TRUE)

labels = c("8g device", "extrapolated 2g signal", "8g device cropped to 2g")
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
  theme_minimal(base_size = 18, base_family = "Times New Roman") +
  scale_color_manual(values = c("gray", 'black', 'black'),labels = labels) +
  scale_linetype_manual(values = c("solid", "solid", "solid"),labels = labels) +
  scale_alpha_manual(values = c(0.8, 0.4, 1), labels = labels) +
  guides(
    colour = guide_legend(title = ""),
    linetype = guide_legend(title = ""),
    alpha = guide_legend(title = "")
  ) +
  xlab("time (s)") +
  ylab("X-axis acceleration (g)") +
  theme(legend.position = "bottom")

ggsave(
  path = file.path("reproduce/figure/"),
  filename = "extrapolate_example_jumpingjack_2g.png",
  plot = p3,
  scale = 2,
  width = 4,
  height = 2
)

return(jj_rate)
}

example_frisbee = function(){
filename = "reproduce/extdata/frisbee_maxed_out.rds"
frisbee_maxed_out = readRDS(filename)

maxed_out_fb = frisbee_maxed_out %>% dplyr::filter(GRANGE == 2)
gt_fb = frisbee_maxed_out %>% dplyr::filter(GRANGE == 8)

# clip
st = maxed_out_fb[1, 1] + 22
maxed_out_fb = maxed_out_fb %>% MIMSunit::clip_data(start_time = st, stop_time = st + 10)
gt_fb = gt_fb %>% MIMSunit::clip_data(start_time = st, stop_time = st + 10)

extrap_fb = extrapolate(
  maxed_out_fb[, 1:2],
  range = c(-2, 2),
  noise_level = noise_level,
  k = k,
  spar = spar
)

fb_rate = Counts::extrapolate_rate(maxed_out_fb, gt_fb, extrap_fb)

# plot
#
forPlot = rbind(
  cbind(gt_fb[, 1:2], group = "gt"),
  cbind(extrap_fb, group = "extrap"),
  cbind(maxed_out_fb[, 1:2], group = "origin")
)

write.csv(x = forPlot, file = "reproduce/table/extrapolation_examples_frisbee.csv", append = F, quote = FALSE, row.names = FALSE, col.names = TRUE)

labels = c("8g device", "extrapolated 2g signal", "8g device cropped to 2g")
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
  theme_minimal(base_size = 18, base_family = "Times New Roman") +
  scale_color_manual(values = c("gray", 'black', 'black'),labels = labels) +
  scale_linetype_manual(values = c("solid", "solid", "solid"),labels = labels) +
  scale_alpha_manual(values = c(0.8, 0.4, 1), labels = labels) +
  guides(
    colour = guide_legend(title = ""),
    linetype = guide_legend(title = ""),
    alpha = guide_legend(title = "")
  ) +
  xlab("time (s)") +
  ylab("X-axis acceleration (g)") +
  theme(legend.position = "bottom")

ggsave(
  path = file.path("reproduce/figure/"),
  filename = "extrapolate_example_frisbee_2g.png",
  plot = p4,
  scale = 2,
  width = 4,
  height = 2
)

return(fb_rate)
}

example_running2 = function(){
noise_level = 0.05
filename = "reproduce/extdata/running2_maxed_out.rds"
running2_maxed_out = readRDS(filename)

maxed_out_r2 = running2_maxed_out %>% dplyr::filter(GRANGE == 2)
gt_r2 = running2_maxed_out %>% dplyr::filter(GRANGE == 8)

# clip
st = maxed_out_r2[1, 1]
maxed_out_r2 = maxed_out_r2 %>% MIMSunit::clip_data(start_time = st, stop_time = st + 8)
gt_r2 = gt_r2 %>% MIMSunit::clip_data(start_time = st, stop_time = st + 8)

extrap_r2 = extrapolate(
  maxed_out_r2[, 1:4],
  range = c(-2, 2),
  noise_level = noise_level,
  k = k,
  spar = spar
)

extrap_r2 = MIMSunit::clip_data(extrap_r2, start_time = extrap_r2[1,1], stop_time = extrap_r2[nrow(extrap_r2),1] - 0.5)
gt_r2 = MIMSunit::clip_data(gt_r2, start_time = gt_r2[1,1], stop_time = gt_r2[nrow(gt_r2),1] - 0.5)
maxed_out_r2 = MIMSunit::clip_data(maxed_out_r2, start_time = maxed_out_r2[1,1], stop_time = maxed_out_r2[nrow(maxed_out_r2),1] - 0.5)

running2_rate = Counts::extrapolate_rate(maxed_out_r2[c(1,3)], gt_r2[c(1,3)], extrap_r2[c(1,3)])

# plot
#
forPlot = rbind(
  cbind(gt_r2[, c(1, 3)], group = "gt"),
  cbind(extrap_r2[, c(1, 3)], group = "extrap"),
  cbind(maxed_out_r2[, c(1, 3)], group = "origin")
)

write.csv(x = forPlot, file = "reproduce/table/extrapolation_examples_running2.csv", append = F, quote = FALSE, row.names = FALSE, col.names = TRUE)

labels = c("8g device", "extrapolated 2g signal", "8g device cropped to 2g")
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
  theme_minimal(base_size = 16, base_family = "Times New Roman") +
  scale_color_manual(values = c("gray", 'black', 'black'),labels = labels) +
  scale_linetype_manual(values = c("solid", "solid", "solid"),labels = labels) +
  scale_alpha_manual(values = c(0.8, 0.4, 1), labels = labels) +
  guides(
    colour = guide_legend(title = ""),
    linetype = guide_legend(title = ""),
    alpha = guide_legend(title = "")
  ) +
  xlab("time (s)") +
  ylab("Y-axis acceleration (g)") +
  theme(legend.position = "bottom")

ggsave(
  path = file.path("reproduce/figure/"),
  filename = "extrapolate_example_running_2_2g.png",
  plot = p5,
  scale = 2,
  width = 4,
  height = 2
)

return(running2_rate)
}

example_running3 = function(){
filename = "reproduce/extdata/treadmill1.rds"
walkrun1 = readRDS(filename)

gt_running = walkrun1 %>% dplyr::filter(
  SR == "40" &
    MPH == 8 &
    WEIGHTS == "0" &
    LOCATION == "NondominantWrist"
)
maxed_out_running = gt_running %>% MIMSunit::cut_off_signal(range = c(-2, 2), noise_std = 0.01)
extrap_running = extrapolate(
  maxed_out_running[, 1:4],
  range = c(-2, 2),
  noise_level = noise_level,
  k = k,
  spar = spar
)

running3_rate = Counts::extrapolate_rate(maxed_out_running[c(1,3)], gt_running[c(1,3)], extrap_running[c(1,3)])

startTime = extrap_running[1, 1]
extrap_clip = extrap_running[, c(1, 3)] %>% MIMSunit::clip_data(start_time = startTime, stop_time = startTime + 8)
maxedout_clip = maxed_out_running[, c(1, 3)] %>% MIMSunit::clip_data(start_time = startTime, stop_time = startTime + 8)
gt_clip = gt_running[, c(1, 3)] %>% MIMSunit::clip_data(start_time = startTime, stop_time = startTime + 8)

# plot
#
forPlot = rbind(
  cbind(gt_clip, group = "gt"),
  cbind(extrap_clip, group = "extrap"),
  cbind(maxedout_clip, group = "origin")
)
write.csv(x = forPlot, file = "reproduce/table/extrapolation_examples_running3.csv", append = F, quote = FALSE, row.names = FALSE, col.names = TRUE)

labels = c("GT9X (40Hz, 8g)", "Extrapolated GT9X", "GT9X cropped to 2g")
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
  theme_minimal(base_size = 16, base_family = "Times New Roman") +
  scale_color_manual(values = c("gray", 'black', 'black'),labels = labels) +
  scale_linetype_manual(values = c("solid", "solid", "solid"),labels = labels) +
  scale_alpha_manual(values = c(0.8, 0.4, 1), labels = labels) +
  guides(
    colour = guide_legend(title = ""),
    linetype = guide_legend(title = ""),
    alpha = guide_legend(title = "")
  ) +
  xlab("time (s)") +
  ylab("Y-axis acceleration (g)") +
  theme(legend.position = "bottom")

ggsave(
  path = file.path("reproduce/figure/"),
  filename = "extrapolate_example_running_3_2g.png",
  plot = p2,
  scale = 2,
  width = 4,
  height = 2
)

return(running3_rate)
}

example_running4 = function(){
  filename = "reproduce/extdata/treadmill1.rds"
  walkrun1 = readRDS(filename)

  gt_running = walkrun1 %>% dplyr::filter(
    SR == "100" &
      MPH == 8 &
      WEIGHTS == "0" &
      SUBJECT == "P1" & SESSION == "1" & LOCATION == "NondominantWrist"
  )
  maxed_out_running = gt_running %>% MIMSunit::cut_off_signal(range = c(-2, 2), noise_std = 0.01)
  extrap_running = MIMSunit::extrapolate(
    maxed_out_running[, 1:4],
    range = c(-2, 2),
    noise_level = noise_level,
    k = k,
    spar = spar
  )

  running4_rate = Counts::extrapolate_rate(maxed_out_running[c(1,3)], gt_running[c(1,3)], extrap_running[c(1,3)])

  startTime = extrap_running[1, 1]
  extrap_clip = extrap_running[, c(1, 3)] %>% MIMSunit::clip_data(start_time = startTime, stop_time = startTime + 10)
  maxedout_clip = maxed_out_running[, c(1, 3)] %>% MIMSunit::clip_data(start_time = startTime, stop_time = startTime + 10)
  gt_clip = gt_running[, c(1, 3)] %>% MIMSunit::clip_data(start_time = startTime, stop_time = startTime + 10)

  # plot
  #
  forPlot = rbind(
    cbind(gt_clip, group = "gt"),
    cbind(extrap_clip, group = "extrap"),
    cbind(maxedout_clip, group = "origin")
  )

  write.csv(x = forPlot, file = "reproduce/table/extrapolation_examples_running4.csv", append = F, quote = FALSE, row.names = FALSE, col.names = TRUE)

  labels = c("GT9X (100Hz, 8g)", "Extrapolated GT9X", "GT9X cropped to 2g")
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
    theme_minimal(base_size = 16, base_family = "Times New Roman") +
    scale_color_manual(values = c("gray", 'black', 'black'),labels = labels) +
    scale_linetype_manual(values = c("solid", "solid", "solid"),labels = labels) +
    scale_alpha_manual(values = c(0.8, 0.4, 1), labels = labels) +
    guides(
      colour = guide_legend(title = ""),
      linetype = guide_legend(title = ""),
      alpha = guide_legend(title = "")
    ) +
    xlab("time (s)") +
    ylab("Y-axis acceleration (g)") +
    theme(legend.position = "bottom")

  ggsave(
    path = file.path("reproduce/figure/"),
    filename = "extrapolate_example_running_4_2g.png",
    plot = p2,
    scale = 2,
    width = 4,
    height = 2
  )

  return(running4_rate)
}

shaker_rate = example_shaker_3g()
example_shaker()
jj_rate = example_jumpingjack()
fb_rate = example_frisbee()
r1_rate = example_running1()
r2_rate = example_running2()
r3_rate = example_running3()
r4_rate = example_running4()
#
print(shaker_rate)
print(jj_rate)
print(fb_rate)
print(r1_rate)
print(r2_rate)
print(r3_rate)
print(r4_rate)

rate_result = data.frame(shaker = shaker_rate, jumpingjack = jj_rate, run_with_ankle = r2_rate, run_with_wrist = r3_rate)

write.csv(rate_result, "reproduce/table/extrapolation_examples_rate.csv", quote = FALSE, row.names = FALSE)
