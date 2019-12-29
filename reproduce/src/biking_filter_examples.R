require(plyr)
require(ggplot2)
require(dplyr)
require(caTools)
filename = "reproduce/extdata/biking_examples.rds"
biking_examples = readRDS(filename)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

colors = gg_color_hue(3)

# filtered biking data
filtered_biking_examples = biking_examples %>% ddply(.(ACTIVITY, LOCATION), function(rows){
  df = rows[,1:4]
  sr = rows$SR[1]
  filtered_df = MIMSunit::iir(df=df, Fs = sr, Fc = c(0.2, 5), order=4, filter_type = 'butter', type = 'pass')
  colnames(filtered_df) = c('HEADER_TIME_STAMP', 'X', 'Y', 'Z')
  st = filtered_df[1,1] + 30
  et = st + 15
  filtered_df = MIMSunit::clip_data(filtered_df, start_time = st, stop_time = et)
  filtered_df$SR = sr
  filtered_df$GRANGE = rows$GRANGE[1]
  filtered_df$TYPE = rows$TYPE[1]
  return(filtered_df)
})

original_biking_examples = biking_examples %>% ddply(.(ACTIVITY, LOCATION), function(rows){
  df = rows[,1:4]
  sr = rows$SR[1]
  st = df[1,1] + 30
  et = st + 15
  colnames(df) = c('HEADER_TIME_STAMP', 'X', 'Y', 'Z')
  selected_df = MIMSunit::clip_data(df, start_time = st, stop_time = et)
  selected_df$SR = sr
  selected_df$GRANGE = rows$GRANGE[1]
  selected_df$TYPE = rows$TYPE[1]
  return(selected_df)
})

original_biking_examples$GROUP = 'Original'
filtered_biking_examples$GROUP = 'Filtered'

biking_data = rbind(original_biking_examples, filtered_biking_examples)

write.csv(biking_data, file = "reproduce/table/biking_filter_examples.csv", append = FALSE, row.names = FALSE, quote = FALSE)

p = ggplot(
  data = biking_data,
  aes(x = HEADER_TIME_STAMP, y = X, colour = GROUP)
) + geom_line(lwd = 1) +
  scale_color_manual(
    values = c( colors[1],"gray")
  ) +
  ylim(-2, 2) +
  xlab("Time (s)") +
  ylab("X-axis Acceleration (g)") +
  theme_minimal(base_size = 16) +
  guides(colour = guide_legend(title = "")) +
  theme(legend.position = "bottom") + facet_wrap(LOCATION ~ ACTIVITY, scales = 'free_x')

ggsave(filename = 'reproduce/figure/biking_filter_examples.png', plot = p, scale = 1, width = 10, height = 6, dpi = 300)