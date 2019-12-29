require(ggplot2)
require(plyr)
require(dplyr)
require(extrafont)
font_import()
loadfonts(device='postscript')
k = 0.05
spar = 0.6
confidence = 0.5
noise_level = 0.03

filename = "reproduce/extdata/jumping_jack_maxed_out.rds"
jumping_jack_maxed_out = readRDS(filename)

# device 0: 80Hz, 8g
device0 = jumping_jack_maxed_out %>% dplyr::filter(GRANGE == 8)
device0 = device0[,1:2]

# # add some impulse
spike_values = -c(-0.023, -0.789,-2.488,-3.854, -2.532 -1.883,-0.879,-0.566,-0.136, 0, 0.24, 1.2
)
spike_x = seq(0, 0.12, 0.01)
spike_x_80 = seq(0, 0.12, 1/80)
spike_values_80 = signal::interp1(spike_x, spike_values, spike_x_80, method='linear')
spike_values_80[9:10] = c(-0.823, -1.234)
begin_value = device0[16, 2]
device0[16:25,2] = spike_values_80
# device 1: 40Hz, 2g
device1 = simulate_new_data(device0, c(-2,2), 40)
# device 2: 30Hz, 4g
device2 = simulate_new_data(device0, c(-4,4), 30)
# device 3: 20Hz, 2g
device3 = simulate_new_data(device0, c(-2,2), 20)

start_time =  device0[[1,1]]
stop_time = start_time + 1

current_run = format(Sys.time(), "%Y%m%d%H")

dir.create(paste0('reproduce/figure/conceptual_diagram/',current_run, '/'))

plotting = function(data, point_size=0.3,
                    plot_point=T,
                    line_size=0.3,
                    plot_line=T,
                    range=c(-2,2),
                    plot_maxed_out_line=T,
                    plot_origin=T) {
    colnames(data) = c('HEADER_TIME_STAMP', 'value')
    p = ggplot(data = data, aes(x=HEADER_TIME_STAMP, y=value))
    if(plot_line){
      p = p + geom_line(size=line_size)
    }
    if(plot_point){
      p = p + geom_point(size=point_size)
    }
    if(plot_maxed_out_line){
      p = p + geom_hline(yintercept = range[1], size=0.3, linetype='dashed')
      p = p + geom_hline(yintercept = range[2], size=0.3, linetype='dashed')
    }
    if(plot_origin){
      p = p + geom_hline(yintercept = 0, size=0.1)
    }
    p = p + theme_bw(base_size = 18, base_family = "Times New Roman") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), plot.margin = unit(c(0, 0, -0.32, -0.32), "line")) + ylim(-5.5, 5.5) + labs(x=NULL, y=NULL)
    return(p)
}

generate_diagram = function(data, device_name, range, sr, start_time, stop_time, extrapolation_detail=F){
  colnames(data) = c('HEADER_TIME_STAMP', 'value')
  plot_data = MIMSunit::clip_data(data, start_time = start_time, stop_time = stop_time)
  # original signal
  p0 = plotting(plot_data, range=range)
  ggsave(filename = paste0('reproduce/figure/conceptual_diagram/',current_run, '/',device_name,'_0.eps'), plot = p0, dpi = 1500, width = 2, height = 1, scale=1)
  ggsave(filename = paste0('reproduce/figure/conceptual_diagram/',current_run, '/',device_name,'_0.png'), plot = p0, dpi = 100, width = 2, height = 1, scale=1)
  # oversampled signal
  oversampled = data.frame(.extrapolate.oversampling(data$HEADER_TIME_STAMP, data$value))
  colnames(oversampled) = c('HEADER_TIME_STAMP', 'value')
  plot_data = MIMSunit::clip_data(oversampled, start_time = start_time, stop_time = stop_time)
  p1 = plotting(plot_data, range=range)
  ggsave(filename = paste0('reproduce/figure/conceptual_diagram/',current_run, '/', device_name,'_1.eps'), plot = p1, dpi = 1500, width = 2, height = 1, scale=1)
  ggsave(filename = paste0('reproduce/figure/conceptual_diagram/',current_run, '/', device_name,'_1.png'), plot = p1, dpi = 100, width = 2, height = 1, scale=1)

  # markers
  markers = .extrapolate.markregion(oversampled$HEADER_TIME_STAMP, oversampled$value, range = range)
  markers_df = data.frame(HEADER_TIME_STAMP=oversampled$HEADER_TIME_STAMP, value=abs(markers))
  plot_markers_data = MIMSunit::clip_data(markers_df, start_time = start_time, stop_time = stop_time)
  p2_1 = p1 +
    geom_line(data = plot_markers_data, aes(x=HEADER_TIME_STAMP, y=value+4.3), size=0.3) +
    geom_hline(yintercept = 4, size=0.3)
  ggsave(filename = paste0('reproduce/figure/conceptual_diagram/',current_run, '/',device_name,'_2_1.eps'), plot = p2_1, dpi = 1500, width = 2, height = 1, scale=1)
  ggsave(filename = paste0('reproduce/figure/conceptual_diagram/',current_run, '/',device_name,'_2_1.png'), plot = p2_1, dpi = 100, width = 2, height = 1, scale=1)

  #neighbors
  neighbors = .extrapolate.neighbor(markers, 100, k)
  if(nrow(neighbors) > 0){
    left_indices = c()
    right_indices = c()
    mo_indices = c()
    for (i in 1:nrow(neighbors)){
      left_indices = c(left_indices, neighbors$left_start[i]:neighbors$left_end[i])
      right_indices = c(right_indices, neighbors$right_start[i]:neighbors$right_end[i])
      mo_indices = c(mo_indices, neighbors$left_end[i]:neighbors$right_start[i])
    }
    left_neighbors = oversampled[left_indices,]
    right_neighbors = oversampled[right_indices,]
    oversampled_nn = oversampled[c(-left_indices,-right_indices),]
    oversampled_nn_nm = oversampled[c(-left_indices,-right_indices, -mo_indices),]
    mo_points = oversampled[mo_indices,]
    mo_points$weight = markers_df[mo_indices,'value']
    left_neighbors = MIMSunit::clip_data(left_neighbors, start_time = start_time, stop_time = stop_time)
    right_neighbors = MIMSunit::clip_data(right_neighbors, start_time = start_time, stop_time = stop_time)
    plot_data = MIMSunit::clip_data(oversampled_nn, start_time = start_time, stop_time = stop_time)
    p2_2 = plotting(plot_data,plot_line=F, range=range) +
      geom_line(data = plot_markers_data, aes(x=HEADER_TIME_STAMP, y=value+4.3), size=0.3) +
      geom_hline(yintercept = 4, size=0.3) +
      geom_point(data=left_neighbors, aes(x=HEADER_TIME_STAMP, y=value), shape=1, size=1) +
      geom_point(data=right_neighbors, aes(x=HEADER_TIME_STAMP, y=value), shape=1, size=1)
    ggsave(filename = paste0('reproduce/figure/conceptual_diagram/',current_run, '/', device_name,'_2_2.eps'), plot = p2_2, dpi = 1500, width = 2, height = 1, scale=1)
    ggsave(filename = paste0('reproduce/figure/conceptual_diagram/',current_run, '/', device_name,'_2_2.png'), plot = p2_2, dpi = 100, width = 2, height = 1, scale=1)
    # fitted lines
    fitted_result = .extrapolate.fitline(oversampled$HEADER_TIME_STAMP, oversampled$value, neighbors, markers, spar, 100, k)
    colnames(fitted_result) = c('HEADER_TIME_STAMP', 'value', 'type', 'index')
    plot_fitted_data = MIMSunit::clip_data(fitted_result, start_time = start_time, stop_time = stop_time)
    points_ex = plot_fitted_data[plot_fitted_data$type == 'point',c(1,2)]

    p2 = plotting(plot_data,plot_line=F, range=range) +
      # geom_line(data = plot_markers_data, aes(x=HEADER_TIME_STAMP, y=value+4.3), size=0.3) +
      # geom_hline(yintercept = 4, size=0.3) +
      geom_point(data=left_neighbors, aes(x=HEADER_TIME_STAMP, y=value), shape=1, size=1) +
      geom_point(data=right_neighbors, aes(x=HEADER_TIME_STAMP, y=value), shape=1, size=1) +
      geom_point(data = points_ex, aes(x=HEADER_TIME_STAMP, y=value), shape=17, size=1.5) +
      geom_vline(xintercept = as.numeric(points_ex$HEADER_TIME_STAMP), size=0.3, linetype='dotted')
    indices = unique(plot_fitted_data$index)
    for(i in indices){
      p2 = p2 + geom_line(data = plot_fitted_data[plot_fitted_data$index == i & plot_fitted_data$type == 'left_line',c(1,2)], aes(x=HEADER_TIME_STAMP, y=value),size=0.2) +
        geom_line(data = plot_fitted_data[plot_fitted_data$index == i & plot_fitted_data$type == 'right_line',c(1,2)], aes(x=HEADER_TIME_STAMP, y=value),size=0.2)
    }

    ggsave(filename = paste0('reproduce/figure/conceptual_diagram/',current_run, '/', device_name,'_2.eps'), plot = p2, dpi = 1500, width = 2, height = 1, scale=1)
    ggsave(filename = paste0('reproduce/figure/conceptual_diagram/',current_run, '/', device_name,'_2.png'), plot = p2, dpi = 100, width = 2, height = 1, scale=1)
    # zoomed
    plot_data_zoomed = MIMSunit::clip_data(oversampled_nn_nm, start_time = start_time + 0.5, stop_time = start_time + 0.8)
    plot_markers_data_zoomed = MIMSunit::clip_data(markers_df, start_time = start_time + 0.5, stop_time = start_time + 0.8)
    mo_points_zoomed = MIMSunit::clip_data(mo_points, start_time = start_time + 0.5, stop_time = start_time + 0.8)
    left_neighbors_zoomed = MIMSunit::clip_data(left_neighbors, start_time = start_time + 0.5, stop_time = start_time + 0.8)
    right_neighbors_zoomed = MIMSunit::clip_data(right_neighbors, start_time = start_time + 0.5, stop_time = start_time + 0.8)
    plot_fitted_data_zoomed = MIMSunit::clip_data(fitted_result, start_time = start_time + 0.5, stop_time = start_time + 0.8)
    points_ex_zoomed = plot_fitted_data_zoomed[plot_fitted_data_zoomed$type == 'point',c(1,2)]
    p2_zoomed = plotting(plot_data_zoomed,plot_line=F, range=range) + ylim(-5.5, 0) +
      geom_point(data=mo_points_zoomed, aes(x=HEADER_TIME_STAMP, y=value, color=weight), size=2) + scale_colour_continuous(low = '#cccccc', high='#333333', na.value = "white", guide=FALSE) +
      geom_point(data=left_neighbors_zoomed, aes(x=HEADER_TIME_STAMP, y=value), shape=1, size=2) +
      geom_point(data=right_neighbors_zoomed, aes(x=HEADER_TIME_STAMP, y=value), shape=1, size=2) +
      geom_point(data = points_ex_zoomed, aes(x=HEADER_TIME_STAMP, y=value), shape=17, size=2) +
      geom_vline(xintercept = as.numeric(points_ex_zoomed$HEADER_TIME_STAMP), size=0.3, linetype='dotted')
    indices = unique(plot_fitted_data_zoomed$index)
    for(i in indices){
      p2_zoomed = p2_zoomed + geom_line(data = plot_fitted_data_zoomed[plot_fitted_data_zoomed$index == i & plot_fitted_data_zoomed$type == 'left_line',c(1,2)], aes(x=HEADER_TIME_STAMP, y=value),size=0.2) +
        geom_line(data = plot_fitted_data_zoomed[plot_fitted_data_zoomed$index == i & plot_fitted_data_zoomed$type == 'right_line',c(1,2)], aes(x=HEADER_TIME_STAMP, y=value),size=0.2)
    }

    ggsave(filename = paste0('reproduce/figure/conceptual_diagram/',current_run, '/', device_name,'_2_zoomed.eps'), plot = p2_zoomed, dpi = 1500, width = 2, height = 1, scale=1)
    ggsave(filename = paste0('reproduce/figure/conceptual_diagram/',current_run, '/', device_name,'_2_zoomed.png'), plot = p2_zoomed, dpi = 100, width = 2, height = 1, scale=1)

    # points to interpolate
    colnames(points_ex) = c('t_ex', 'value_ex')
    t_mark = oversampled$HEADER_TIME_STAMP[abs(markers) < 0.5]
    value_mark = oversampled$value[abs(markers) < 0.5]
    to_extrapolate_data = data.frame(HEADER_TIME_STAMP=t_mark, value=value_mark)
    plot_to_extrapolate_data = MIMSunit::clip_data(to_extrapolate_data, start_time, stop_time, 'sensor')
    p2_3 = plotting(plot_to_extrapolate_data, plot_line = FALSE, range=range) +
      geom_point(data = points_ex, aes(x=t_ex, y=value_ex), shape=17, size=1)
    ggsave(filename = paste0('reproduce/figure/conceptual_diagram/', current_run, '/', device_name,'_2_3.eps'), plot = p2_3, dpi = 1500, width = 2, height = 1, scale=1)
    ggsave(filename = paste0('reproduce/figure/conceptual_diagram/', current_run, '/', device_name,'_2_3.png'), plot = p2_3, dpi = 100, width = 2, height = 1, scale=1)
    # extrapolated and interpolated
    extrapolated = .extrapolate.interpolate(oversampled$HEADER_TIME_STAMP,oversampled$value, markers, points_ex, 100)
    colnames(extrapolated) = c('HEADER_TIME_STAMP', 'value')
    plot_extrapolated = MIMSunit::clip_data(extrapolated, start_time, stop_time, "sensor")

    p3 = plotting(plot_extrapolated, range=range) + geom_point(data = points_ex, aes(x=as.numeric(t_ex), y=value_ex), shape=17, size=2)
    ggsave(filename = paste0('reproduce/figure/conceptual_diagram/', current_run, '/', device_name,'_3.eps'), plot = p3, dpi = 1500, width = 2, height = 1, scale=1)
    ggsave(filename = paste0('reproduce/figure/conceptual_diagram/', current_run, '/', device_name,'_3.png'), plot = p3, dpi = 100, width = 2, height = 1, scale=1)
  }else{
    extrapolated = oversampled
  }

  # filtering
  sr = sampling_rate(extrapolated)
  filtered = iir(
    extrapolated,
    Fs = sr,
    Fc = c(0.2, 5),
    order = 4,
    type = "pass",
    filter_type = "butter"
  )
  plot_filtered = MIMSunit::clip_data(filtered, start_time, stop_time, "sensor")
  colnames(plot_filtered) = c('HEADER_TIME_STAMP', 'value')
  p4 = plotting(plot_filtered, plot_maxed_out_line = F)
  ggsave(filename = paste0('reproduce/figure/conceptual_diagram/', current_run, '/', device_name,'_4.eps'), plot = p4, dpi = 1500, width = 2, height = 1, scale=1)
  ggsave(filename = paste0('reproduce/figure/conceptual_diagram/', current_run, '/', device_name,'_4.png'), plot = p4, dpi = 100, width = 2, height = 1, scale=1)
  # aggregation
  filtered$HEADER_TIME_STAMP = as.POSIXct(filtered$HEADER_TIME_STAMP, origin="1970-01-01")
  agg = filtered
  colnames(agg) = c('HEADER_TIME_STAMP', 'value')
  agg$value = abs(agg$value)
  counts = Counts::aggregate(
    filtered,
    breaks = '1 sec',
    type = 'trapz',
    rectify = TRUE
  )
  second_count_value = counts[1,2]
  minute_count_value = second_count_value * 60
  plot_agg =  MIMSunit::clip_data(agg, start_time, stop_time, "sensor")
  label1 = paste0("Area under curve: ", round(second_count_value, 2))
  label2 = paste0("Equiv. to ", round(minute_count_value, 1), ' MIMS-unit/min')
  p5 = plotting(plot_agg, plot_maxed_out_line = F, plot_point = F) +
    geom_area() +
    geom_text(data=data.frame(x=start_time, y=-3.5, label=label1),aes(x=x,y=y,label=label),hjust = 0, size=3, family="Times New Roman") +
    geom_text(data=data.frame(x=start_time, y=-5, label=label2),aes(x=x,y=y,label=label),hjust = 0, size=3, family="Times New Roman")
  ggsave(filename = paste0('reproduce/figure/conceptual_diagram/', current_run, '/', device_name,'_5.eps'), plot = p5, dpi = 1500, width = 2, height = 1, scale=1)
  ggsave(filename = paste0('reproduce/figure/conceptual_diagram/', current_run, '/', device_name,'_5.png'), plot = p5, dpi = 100, width = 2, height = 1, scale=1)
}

generate_diagram(data = device0, device_name = "device0", range = c(-8, 8), sr = 80, start_time = start_time, stop_time = stop_time, extrapolation_detail = T)
generate_diagram(data = device1, device_name = "device1", range = c(-2, 2), sr = 40, start_time = start_time, stop_time = stop_time, extrapolation_detail = T)
generate_diagram(data = device2, device_name = "device2", range = c(-4, 4), sr = 30, start_time = start_time, stop_time = stop_time, extrapolation_detail = T)
generate_diagram(data = device3, device_name = "device3", range = c(-2, 2), sr = 20, start_time = start_time, stop_time = stop_time, extrapolation_detail = T)


plotting = function(data,
                    point_size = 0.3,
                    plot_point = T,
                    line_size = 0.3,
                    plot_line = T,
                    range = c(-2, 2),
                    plot_maxed_out_line = T,
                    plot_origin = T) {
  colnames(data) = c('HEADER_TIME_STAMP', 'value')
  p = ggplot(data = data, aes(x = .data$HEADER_TIME_STAMP, y = .data$value))
  if (plot_line) {
    p = p + geom_line(size = line_size)
  }
  if (plot_point) {
    p = p + geom_point(size = point_size)
  }
  if (plot_maxed_out_line) {
    p = p + geom_hline(yintercept = range[1],
                       size = 0.3,
                       linetype = 'dashed')
    p = p + geom_hline(yintercept = range[2],
                       size = 0.3,
                       linetype = 'dashed')
  }
  if (plot_origin) {
    p = p + geom_hline(yintercept = 0, size = 0.1)
  }
  p = p + theme_bw(base_size = 18, base_family = "Times New Roman") + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(0, 0, -0.32, -0.32), "line")
  ) + ylim(-5.5, 5.5) + labs(x = NULL, y = NULL)
  return(p)
}

generate_diagram = function(data,
                            device_name,
                            range,
                            sr,
                            start_time,
                            stop_time,
                            extrapolation_detail = F) {
  colnames(data) = c('HEADER_TIME_STAMP', 'value')
  plot_data = MIMSunit::clip_data(
    data,
    start_time = start_time,
    stop_time = stop_time,
  )
  # original signal
  p0 = plotting(plot_data, range = range)
  ggsave(
    filename = paste0(
      'reproduce/figure/conceptual_diagram/',
      current_run,
      '/',
      device_name,
      '_0.eps'
    ),
    plot = p0,
    dpi = 1500,
    width = 2,
    height = 1,
    scale = 1
  )
  ggsave(
    filename = paste0(
      'reproduce/figure/conceptual_diagram/',
      current_run,
      '/',
      device_name,
      '_0.png'
    ),
    plot = p0,
    dpi = 100,
    width = 2,
    height = 1,
    scale = 1
  )
  # oversampled signal
  oversampled = data.frame(.extrapolate.oversampling(data$HEADER_TIME_STAMP, data$value))
  colnames(oversampled) = c('HEADER_TIME_STAMP', 'value')
  plot_data = MIMSunit::clip_data(
    oversampled,
    start_time = start_time,
    stop_time = stop_time,
  )
  p1 = plotting(plot_data, range = range)
  ggsave(
    filename = paste0(
      'reproduce/figure/conceptual_diagram/',
      current_run,
      '/',
      device_name,
      '_1.eps'
    ),
    plot = p1,
    dpi = 1500,
    width = 2,
    height = 1,
    scale = 1
  )
  ggsave(
    filename = paste0(
      'reproduce/figure/conceptual_diagram/',
      current_run,
      '/',
      device_name,
      '_1.png'
    ),
    plot = p1,
    dpi = 100,
    width = 2,
    height = 1,
    scale = 1
  )

  # markers
  markers = MIMSunit::.extrapolate.markregion(oversampled$HEADER_TIME_STAMP, oversampled$value, range = range)
  markers_df = data.frame(HEADER_TIME_STAMP = oversampled$HEADER_TIME_STAMP,
                          value = abs(markers))
  plot_markers_data = MIMSunit::clip_data(
    markers_df,
    start_time = start_time,
    stop_time = stop_time,
  )
  p2_1 = p1 +
    geom_line(data = plot_markers_data,
              aes(x = .data$HEADER_TIME_STAMP, y = .data$value + 4.3),
              size = 0.3) +
    geom_hline(yintercept = 4, size = 0.3)
  ggsave(
    filename = paste0(
      'reproduce/figure/conceptual_diagram/',
      current_run,
      '/',
      device_name,
      '_2_1.eps'
    ),
    plot = p2_1,
    dpi = 1500,
    width = 2,
    height = 1,
    scale = 1
  )
  ggsave(
    filename = paste0(
      'reproduce/figure/conceptual_diagram/',
      current_run,
      '/',
      device_name,
      '_2_1.png'
    ),
    plot = p2_1,
    dpi = 100,
    width = 2,
    height = 1,
    scale = 1
  )

  #neighbors
  neighbors = MIMSunit::.extrapolate_neighbor(markers, 100, k)
  if (nrow(neighbors) > 0) {
    left_indices = c()
    right_indices = c()
    mo_indices = c()
    for (i in 1:nrow(neighbors)) {
      left_indices = c(left_indices,
                       neighbors$left_start[i]:neighbors$left_end[i])
      right_indices = c(right_indices,
                        neighbors$right_start[i]:neighbors$right_end[i])
      mo_indices = c(mo_indices,
                     neighbors$left_end[i]:neighbors$right_start[i])
    }
    left_neighbors = oversampled[left_indices, ]
    right_neighbors = oversampled[right_indices, ]
    oversampled_nn = oversampled[c(-left_indices, -right_indices), ]
    oversampled_nn_nm = oversampled[c(-left_indices, -right_indices, -mo_indices), ]
    mo_points = oversampled[mo_indices, ]
    mo_points$weight = markers_df[mo_indices, 'value']
    left_neighbors = MIMSunit::clip_data(
      left_neighbors,
      start_time = start_time,
      stop_time = stop_time,
    )
    right_neighbors = MIMSunit::clip_data(
      right_neighbors,
      start_time = start_time,
      stop_time = stop_time,
    )
    plot_data = MIMSunit::clip_data(
      oversampled_nn,
      start_time = start_time,
      stop_time = stop_time,
    )
    p2_2 = plotting(plot_data, plot_line = F, range = range) +
      geom_line(data = plot_markers_data,
                aes(x = .data$HEADER_TIME_STAMP, y = .data$value + 4.3),
                size = 0.3) +
      geom_hline(yintercept = 4, size = 0.3) +
      geom_point(
        data = left_neighbors,
        aes(x = .data$HEADER_TIME_STAMP, y = .data$value),
        shape = 1,
        size = 1
      ) +
      geom_point(
        data = right_neighbors,
        aes(x = .data$HEADER_TIME_STAMP, y = .data$value),
        shape = 1,
        size = 1
      )
    ggsave(
      filename = paste0(
        'reproduce/figure/conceptual_diagram/',
        current_run,
        '/',
        device_name,
        '_2_2.eps'
      ),
      plot = p2_2,
      dpi = 1500,
      width = 2,
      height = 1,
      scale = 1
    )
    ggsave(
      filename = paste0(
        'reproduce/figure/conceptual_diagram/',
        current_run,
        '/',
        device_name,
        '_2_2.png'
      ),
      plot = p2_2,
      dpi = 100,
      width = 2,
      height = 1,
      scale = 1
    )
    # fitted lines
    fitted_result = MIMSunit::.extrapolate_fitline(
      oversampled$HEADER_TIME_STAMP,
      oversampled$value,
      neighbors,
      markers,
      spar,
      100,
      k
    )
    colnames(fitted_result) = c('HEADER_TIME_STAMP', 'value', 'type', 'index')
    plot_fitted_data = MIMSunit::clip_data(
      fitted_result,
      start_time = start_time,
      stop_time = stop_time,
    )
    points_ex = plot_fitted_data[plot_fitted_data$type == 'point', c(1, 2)]

    p2 = plotting(plot_data, plot_line = F, range = range) +
      # geom_line(data = plot_markers_data, aes(x=HEADER_TIME_STAMP, y=value+4.3), size=0.3) +
      # geom_hline(yintercept = 4, size=0.3) +
      geom_point(
        data = left_neighbors,
        aes(x = .data$HEADER_TIME_STAMP, y = .data$value),
        shape = 1,
        size = 1
      ) +
      geom_point(
        data = right_neighbors,
        aes(x = .data$HEADER_TIME_STAMP, y = .data$value),
        shape = 1,
        size = 1
      ) +
      geom_point(
        data = points_ex,
        aes(x = .data$HEADER_TIME_STAMP, y = .data$value),
        shape = 17,
        size = 1.5
      ) +
      geom_vline(
        xintercept = as.numeric(points_ex$HEADER_TIME_STAMP),
        size = 0.3,
        linetype = 'dotted'
      )
    indices = unique(plot_fitted_data$index)
    for (i in indices) {
      p2 = p2 + geom_line(data = plot_fitted_data[plot_fitted_data$index == i &
                                                    plot_fitted_data$type == 'left_line', c(1, 2)],
                          aes(x = .data$HEADER_TIME_STAMP, y = .data$value),
                          size = 0.2) +
        geom_line(data = plot_fitted_data[plot_fitted_data$index == i &
                                            plot_fitted_data$type == 'right_line', c(1, 2)],
                  aes(x = .data$HEADER_TIME_STAMP, y = .data$value),
                  size = 0.2)
    }

    ggsave(
      filename = paste0(
        'reproduce/figure/conceptual_diagram/',
        current_run,
        '/',
        device_name,
        '_2.eps'
      ),
      plot = p2,
      dpi = 1500,
      width = 2,
      height = 1,
      scale = 1
    )
    ggsave(
      filename = paste0(
        'reproduce/figure/conceptual_diagram/',
        current_run,
        '/',
        device_name,
        '_2.png'
      ),
      plot = p2,
      dpi = 100,
      width = 2,
      height = 1,
      scale = 1
    )
    # zoomed
    plot_data_zoomed = MIMSunit::clip_data(
      oversampled_nn_nm,
      start_time = start_time + 0.5,
      stop_time = start_time + 0.8,
    )
    plot_markers_data_zoomed = MIMSunit::clip_data(
      markers_df,
      start_time = start_time + 0.5,
      stop_time = start_time + 0.8,
      file_type = "sensor"
    )
    mo_points_zoomed = MIMSunit::clip_data(
      mo_points,
      start_time = start_time + 0.5,
      stop_time = start_time + 0.8,
      file_type = "sensor"
    )
    left_neighbors_zoomed = MIMSunit::clip_data(
      left_neighbors,
      start_time = start_time + 0.5,
      stop_time = start_time + 0.8,
      file_type = "sensor"
    )
    right_neighbors_zoomed = MIMSunit::clip_data(
      right_neighbors,
      start_time = start_time + 0.5,
      stop_time = start_time + 0.8,
      file_type = "sensor"
    )
    plot_fitted_data_zoomed = MIMSunit::clip_data(
      fitted_result,
      start_time = start_time + 0.5,
      stop_time = start_time + 0.8,
      file_type = "sensor"
    )
    points_ex_zoomed = plot_fitted_data_zoomed[plot_fitted_data_zoomed$type == 'point', c(1, 2)]
    p2_zoomed = plotting(plot_data_zoomed, plot_line = F, range = range) + ylim(-5.5, 0) +
      geom_point(
        data = mo_points_zoomed,
        aes(x = .data$HEADER_TIME_STAMP, y = .data$value, color = .data$weight),
        size = 2
      ) + scale_colour_continuous(
        low = '#cccccc',
        high = '#333333',
        na.value = "white",
        guide = FALSE
      ) +
      geom_point(
        data = left_neighbors_zoomed,
        aes(x = .data$HEADER_TIME_STAMP, y = .data$value),
        shape = 1,
        size = 2
      ) +
      geom_point(
        data = right_neighbors_zoomed,
        aes(x = .data$HEADER_TIME_STAMP, y = .data$value),
        shape = 1,
        size = 2
      ) +
      geom_point(
        data = points_ex_zoomed,
        aes(x = .data$HEADER_TIME_STAMP, y = .data$value),
        shape = 17,
        size = 2
      ) +
      geom_vline(
        xintercept = as.numeric(points_ex_zoomed$HEADER_TIME_STAMP),
        size = 0.3,
        linetype = 'dotted'
      )
    indices = unique(plot_fitted_data_zoomed$index)
    for (i in indices) {
      p2_zoomed = p2_zoomed + geom_line(data = plot_fitted_data_zoomed[plot_fitted_data_zoomed$index == i &
                                                                         plot_fitted_data_zoomed$type == 'left_line', c(1, 2)],
                                        aes(x = .data$HEADER_TIME_STAMP, y = .data$value),
                                        size = 0.2) +
        geom_line(data = plot_fitted_data_zoomed[plot_fitted_data_zoomed$index == i &
                                                   plot_fitted_data_zoomed$type == 'right_line', c(1, 2)],
                  aes(x = .data$HEADER_TIME_STAMP, y = .data$value),
                  size = 0.2)
    }

    ggsave(
      filename = paste0(
        'reproduce/figure/conceptual_diagram/',
        current_run,
        '/',
        device_name,
        '_2_zoomed.eps'
      ),
      plot = p2_zoomed,
      dpi = 1500,
      width = 2,
      height = 1,
      scale = 1
    )
    ggsave(
      filename = paste0(
        'reproduce/figure/conceptual_diagram/',
        current_run,
        '/',
        device_name,
        '_2_zoomed.png'
      ),
      plot = p2_zoomed,
      dpi = 100,
      width = 2,
      height = 1,
      scale = 1
    )

    # points to interpolate
    colnames(points_ex) = c('t_ex', 'value_ex')
    t_mark = oversampled$HEADER_TIME_STAMP[abs(markers) < 0.5]
    value_mark = oversampled$value[abs(markers) < 0.5]
    to_extrapolate_data = data.frame(HEADER_TIME_STAMP = t_mark, value =
                                       value_mark)
    plot_to_extrapolate_data = MIMSunit::clip_data(to_extrapolate_data, start_time, stop_time, 'sensor')
    p2_3 = plotting(plot_to_extrapolate_data,
                    plot_line = FALSE,
                    range = range) +
      geom_point(
        data = points_ex,
        aes(x = .data$t_ex, y = .data$value_ex),
        shape = 17,
        size = 1
      )
    ggsave(
      filename = paste0(
        'reproduce/figure/conceptual_diagram/',
        current_run,
        '/',
        device_name,
        '_2_3.eps'
      ),
      plot = p2_3,
      dpi = 1500,
      width = 2,
      height = 1,
      scale = 1
    )
    ggsave(
      filename = paste0(
        'reproduce/figure/conceptual_diagram/',
        current_run,
        '/',
        device_name,
        '_2_3.png'
      ),
      plot = p2_3,
      dpi = 100,
      width = 2,
      height = 1,
      scale = 1
    )
    # extrapolated and interpolated
    extrapolated = MIMSunit::.extrapolate_interpolate(oversampled$HEADER_TIME_STAMP,
                                                      oversampled$value,
                                                      markers,
                                                      points_ex,
                                                      100)
    colnames(extrapolated) = c('HEADER_TIME_STAMP', 'value')
    plot_extrapolated = MIMSunit::clip_data(extrapolated, start_time, stop_time, "sensor")

    p3 = plotting(plot_extrapolated, range = range) + geom_point(
      data = points_ex,
      aes(x = as.numeric(.data$t_ex), y = .data$value_ex),
      shape = 17,
      size = 2
    )
    ggsave(
      filename = paste0(
        'reproduce/figure/conceptual_diagram/',
        current_run,
        '/',
        device_name,
        '_3.eps'
      ),
      plot = p3,
      dpi = 1500,
      width = 2,
      height = 1,
      scale = 1
    )
    ggsave(
      filename = paste0(
        'reproduce/figure/conceptual_diagram/',
        current_run,
        '/',
        device_name,
        '_3.png'
      ),
      plot = p3,
      dpi = 100,
      width = 2,
      height = 1,
      scale = 1
    )
  } else{
    extrapolated = oversampled
  }

  # filtering
  sr = sampling_rate(extrapolated)
  filtered = MIMSunit::iir(
    extrapolated,
    Fs = sr,
    Fc = c(0.2, 5),
    order = 4,
    type = "pass",
    filter_type = "butter"
  )
  plot_filtered = MIMSunit::clip_data(filtered, start_time, stop_time, "sensor")
  colnames(plot_filtered) = c('HEADER_TIME_STAMP', 'value')
  p4 = plotting(plot_filtered, plot_maxed_out_line = F)
  ggsave(
    filename = paste0(
      'reproduce/figure/conceptual_diagram/',
      current_run,
      '/',
      device_name,
      '_4.eps'
    ),
    plot = p4,
    dpi = 1500,
    width = 2,
    height = 1,
    scale = 1
  )
  ggsave(
    filename = paste0(
      'reproduce/figure/conceptual_diagram/',
      current_run,
      '/',
      device_name,
      '_4.png'
    ),
    plot = p4,
    dpi = 100,
    width = 2,
    height = 1,
    scale = 1
  )
  # aggregation
  filtered$HEADER_TIME_STAMP = as.POSIXct(filtered$HEADER_TIME_STAMP, origin =
                                            "1970-01-01")
  agg = filtered
  colnames(agg) = c('HEADER_TIME_STAMP', 'value')
  agg$value = abs(agg$value)
  counts = Counts::aggregate(filtered,
                             breaks = '1 sec',
                             type = 'trapz',
                             rectify = TRUE)
  second_count_value = counts[1, 2]
  minute_count_value = second_count_value * 60
  plot_agg =  MIMSunit::clip_data(agg, start_time, stop_time, "sensor")
  label1 = paste0("Area under curve: ", round(second_count_value, 2))
  label2 = paste0("Equiv. to ", round(minute_count_value, 1), ' MIMS-unit/min')
  p5 = plotting(plot_agg,
                plot_maxed_out_line = F,
                plot_point = F) +
    geom_area() +
    geom_text(
      data = data.frame(x = start_time, y = -3.5, label = label1),
      aes(x = x, y = y, label = label),
      hjust = 0,
      size = 3,
      family = "Times New Roman"
    ) +
    geom_text(
      data = data.frame(x = start_time, y = -5, label = label2),
      aes(x = x, y = y, label = label),
      hjust = 0,
      size = 3,
      family = "Times New Roman"
    )
  ggsave(
    filename = paste0(
      'reproduce/figure/conceptual_diagram/',
      current_run,
      '/',
      device_name,
      '_5.eps'
    ),
    plot = p5,
    dpi = 1500,
    width = 2,
    height = 1,
    scale = 1
  )
  ggsave(
    filename = paste0(
      'reproduce/figure/conceptual_diagram/',
      current_run,
      '/',
      device_name,
      '_5.png'
    ),
    plot = p5,
    dpi = 100,
    width = 2,
    height = 1,
    scale = 1
  )
}

generate_diagram(
  data = device0,
  device_name = "device0",
  range = c(-8, 8),
  sr = 80,
  start_time = start_time,
  stop_time = stop_time,
  extrapolation_detail = T
)
generate_diagram(
  data = device1,
  device_name = "device1",
  range = c(-2, 2),
  sr = 40,
  start_time = start_time,
  stop_time = stop_time,
  extrapolation_detail = T
)
generate_diagram(
  data = device2,
  device_name = "device2",
  range = c(-4, 4),
  sr = 30,
  start_time = start_time,
  stop_time = stop_time,
  extrapolation_detail = T
)
generate_diagram(
  data = device3,
  device_name = "device3",
  range = c(-2, 2),
  sr = 20,
  start_time = start_time,
  stop_time = stop_time,
  extrapolation_detail = T
)


usethis::use_data("conceptual_diagram")
