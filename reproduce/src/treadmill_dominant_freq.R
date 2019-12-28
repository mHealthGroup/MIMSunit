require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(mHealthR)
require(Counts)
filename = "reproduce/extdata/treadmill.rds"
treadmill_data = readRDS(filename)

selected_data = treadmill_data %>% filter(GRANGE == "8", ID == "GT9X") %>% select(-START_TIME, -STOP_TIME, -SR, -GRANGE, -ID) %>% melt(id = c("MPH", 'PID', 'LOCATION', 'HEADER_TIME_STAMP'))

dom_freq_data = selected_data %>% ddply(.(PID, MPH, LOCATION, variable), function(chunk){
  selected_chunk = chunk[c('HEADER_TIME_STAMP', 'value')]
  sr = sampling_rate(selected_chunk)
  dom_freqs = .compute_dominant_freq(selected_chunk$value, sr, nth = 2)
  dom_freqs = as.vector(as.matrix(dom_freqs))
  axes_amp = max(abs(selected_chunk$value))
  return(data.frame(MPH=unique(chunk$MPH),
                    PID=unique(chunk$PID),
                    LOCATION=unique(chunk$LOCATION),
                    FIRST_DOM_FREQ = dom_freqs[1],
                    SECOND_DOM_FREQ = dom_freqs[2],
                    FIRST_DOM_FREQ_INTENSITY = dom_freqs[3],
                    SECOND_DOM_FREQ_INTENSITY = dom_freqs[4],
                    AMPLITUDE = axes_amp)
                    )
})

dom_freq_data_freq = dom_freq_data %>% melt(id=c("MPH", 'PID', 'LOCATION', 'FIRST_DOM_FREQ_INTENSITY', 'SECOND_DOM_FREQ_INTENSITY', 'variable'), variable.name='FREQUENCY_TYPE')
dom_freq_data_intense = dom_freq_data %>% melt(id=c("MPH", 'PID', 'LOCATION', 'FIRST_DOM_FREQ', 'SECOND_DOM_FREQ', 'variable'), variable.name='FREQUENCY_INTENSITY_TYPE')
p = ggplot(data = dom_freq_data_freq, aes(x=as.factor(MPH), y=value, fill=LOCATION)) + geom_boxplot(outlier.shape=NA)+ xlab("Speed (mph)") + ylab("Dominant frequency (Hz)") + theme_minimal() + facet_grid(variable ~ FREQUENCY_TYPE)
p2 = ggplot(data = dom_freq_data_intense, aes(x=as.factor(MPH), y=value, fill=LOCATION)) + geom_boxplot(outlier.shape=NA)+ xlab("Speed (mph)") + ylab("Dominant frequency Intensity") + theme_minimal() + facet_grid(variable ~ FREQUENCY_INTENSITY_TYPE)
p3 = ggplot(data = dom_freq_data, aes(x=as.factor(MPH), y=AMPLITUDE, fill=LOCATION)) + geom_boxplot(outlier.shape=NA)+ xlab("Speed (mph)") + ylab("Amplitude Intensity") + theme_minimal() + facet_grid(variable ~ .)
ggsave(filename = "reproduce/figure/treadmill_dominant_freqs.png", plot = p, scale = 1, width = 8, height = 6, dpi = 100)
ggsave(filename = "reproduce/figure/treadmill_dominant_intense.png", plot = p2, scale = 1, width = 8, height = 6, dpi = 100)
ggsave(filename = "reproduce/figure/treadmill_dominant_amp.png", plot = p3, scale = 1, width = 8, height = 6, dpi = 100)
write.csv(x = dominant_freq_summary, file = "reproduce/table/treadmill_dominant_freqs.csv", quote = FALSE, row.names = FALSE, )
