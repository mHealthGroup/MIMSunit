require(ggplot2)

filename = "reproduce/extdata/actigraph_response.rds"

response = readRDS(filename)

response$NF = response$SR / 2

response$SR = paste0(response$SR, " Hz")


p = ggplot(data = response) +
  geom_line(aes(x = FREQ, y = VALUE)) +
  geom_vline(aes(xintercept = NF), linetype = "dashed") +
  facet_wrap(~SR, ncol = 3) +
  theme_bw() +
  xlab("Frequency (Hz)") +
  ylab("Normalized Intensity") +
  coord_cartesian(xlim = c(0, 10)) +
  theme(strip.background = element_blank())


ggsave(filename = "actigraph_response.png", plot = p, path = "reproduce/figure/", scale = 1.5, width = 4, height = 2)
