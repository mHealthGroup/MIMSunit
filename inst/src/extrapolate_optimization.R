require(plyr)
require(dplyr)
require(reshape2)
require(ggplot2)
require(mhealthformatsupportr)
require(signal)
require(flux)
library(doSNOW)
# cl<-makeCluster(4)
# registerDoSNOW(cl)
extrapolate.design = function(freqs = seq(1, 10, by = 1),
                              amps = seq(4, 12, by = 1),
                              noises = seq(0.01, 0.1, by = 0.05),
                              sr = 30, grange = 3, duration = 10, spar = 0.1, k = 0.4){
  paras = expand.grid(freq = freqs, amp = amps, noise = noises, sr = sr, grange = grange, spar = spar, k = k)
  result = paras %>% adply(1, function(para){
    dat = .extrapolate.signal(para$sr, para$grange, para$amp, para$freq, para$noise, duration = duration)
    tryCatch(
      {
        dat_ex = extrapolate(t = dat$t, value = dat$y, range = c(-grange, grange), noise_std = para$noise, k = para$k, spar = para$spar, confident = 0.5, fit_model = "spline")

        dat_up_t = approx(dat$t, dat$y_t, xout = dat_ex$t) %>% as.data.frame %>% na.omit %>% plyr::rename(replace = c(x = "t", y = "value"))
        dat_up = approx(dat$t, dat$y, xout = dat_ex$t) %>% as.data.frame %>% na.omit %>% plyr::rename(replace = c(x = "t", y = "value"))

        # exclude the heading and tailing small segments (impact of step response)
        dat_up = dat_up %>% subset(t > 0.5 & t < duration - 0.5)
        dat_ex = dat_ex %>% subset(t > 0.5 & t < duration - 0.5)

        true_auc = trapz(dat_up_t$t, abs(dat_up_t$value))
        original_err = (true_auc - trapz(dat_up$t, abs(dat_up$value))) / true_auc
        ex_err = (true_auc - trapz(dat_ex$t, abs(dat_ex$value))) / true_auc
        ex_gain = original_err - abs(ex_err)
        return(data.frame(orig_err = original_err, ex_err = ex_err, ex_gain = ex_gain))
      }, error = function(e){
        print(e)
        print(para)
        return(data.frame(orig_err = NULL, ex_err = NULL, ex_gain = NULL))
      }
    )
  }, .inform = TRUE, .progress = "text", .parallel = FALSE)

  return(result)
}

.extrapolate.signal = function(sr, grange, amp, freq, noise, duration = 60, seed = 1){
  x = seq(0, duration, length = duration * sr)
  set.seed(seed)
  y_noise = rnorm(length(x), sd = noise)
  phase = runif(1, min = 0, max = 2*pi)
  y_t = amp*sin(2*pi*freq*x + phase) + y_noise
  y = amp*sin(2*pi*freq*x + phase) + y_noise
  y[y >= grange - noise] = grange + y_noise[y >= grange - noise]
  y[y <= -grange + noise] = -grange + y_noise[y <= -grange + noise]
  result = data.frame(t = x, y = y, y_t = y_t)
  return(result)
}

extrapolate.plot_optimization = function(ex_result){
  # require(ggplot2)
  # ex_result %>% melt(id = c("freq","noise","amp", "sr", "grange", "spar", "k")) %>% ggplot(aes(x = freq, y = value, color = as.factor(amp))) +
  #   geom_line(aes(lty = variable), lwd = 1) +
  #   theme_minimal(base_size = 16) +
  #   xlab("Signal frequency (Hz)") +
  #   ylab("AUC error") +
  #   scale_linetype_discrete(labels = c("Original signal", "Extrapolated signal")) +
  #   guides(colour = guide_legend(title = "Amplitude"), linetype = guide_legend(title = "AUC error")) +
  #   theme(legend.position = "left") +
  #   facet_grid(k ~ spar)
  require(ggplot2)
  ex_result[ex_result['ex_gain'] == max(ex_result['ex_gain']), 'max'] = 1
  p = ggplot(ex_result, aes(x = factor(k), y = factor(spar))) + geom_tile(aes(fill = ex_gain), colour = "grey50") +
    geom_text(aes(label=paste0(ex_gain, "%")), size = 2)+
    scale_fill_gradient(low = "gray30", high = "white", guide = FALSE) +
    xlab('Neighborhood (s)') +
    ylab('Smoothing factor') +
    theme_gray(base_size = 9) +
    theme(panel.background = element_rect(fill = "white"))
}

extrapolate.plot_distribution = function(ex_result){
  # require(ggplot2)
  # ex_result %>% melt(id = c("freq","noise","amp", "sr", "grange", "spar", "k")) %>% ggplot(aes(x = freq, y = value, color = as.factor(amp))) +
  #   geom_line(aes(lty = variable), lwd = 1) +
  #   theme_minimal(base_size = 16) +
  #   xlab("Signal frequency (Hz)") +
  #   ylab("AUC error") +
  #   scale_linetype_discrete(labels = c("Original signal", "Extrapolated signal")) +
  #   guides(colour = guide_legend(title = "Amplitude"), linetype = guide_legend(title = "AUC error")) +
  #   theme(legend.position = "left") +
  #   facet_grid(k ~ spar)
  require(ggplot2)
  p = ggplot(ex_result, aes(x = factor(freq), y = factor(amp))) + geom_tile(aes(fill = ex_gain)) +
    geom_text(aes(label=paste0(ex_gain, "%")), size = 2)+
    scale_fill_gradient(low = "gray30", high = "white", guide = FALSE) +
    # scale_x_discrete() +
    xlab('Frequency (Hz)') +
    ylab('Amplitude (g)') +
    theme_gray(base_size = 9) +
    theme(panel.background = element_rect(fill = "white")) +
    ggtitle(paste("smoothing:", unique(ex_result$spar), ",", "neighborhood:",unique(ex_result$k)))
    # theme_minimal(base_size = 10) +
    # margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
}

# result = extrapolate.design(sr = 40, grange = 3, spar = seq(0.1, 1, by = 0.1), k = seq(0.05, 1, by = 0.05), duration = 30, freqs = seq(1,10,by = 1), amps = c(3.5, seq(4, 10, by = 1)), noises = seq(0.01, 0.1, by = 0.05))

result1 = extrapolate.design(sr = 40, grange = 2, spar = seq(0, 1, by = 0.1), k = seq(0.05, 1, by = 0.05), duration = 30, freqs = seq(1,5, by = 1), amps = 2, noises = seq(0.01, 0.1, by = 0.05))
save.image()
result2 = extrapolate.design(sr = 40, grange = 2, spar = seq(0, 1, by = 0.1), k = seq(0.05, 1, by = 0.05), duration = 30, freqs = seq(1,5, by = 1), amps = 3, noises = seq(0.01, 0.1, by = 0.05))
save.image()
result3 = extrapolate.design(sr = 40, grange = 2, spar = seq(0, 1, by = 0.1), k = seq(0.05, 1, by = 0.05), duration = 30, freqs = seq(1,5, by = 1), amps = 4, noises = seq(0.01, 0.1, by = 0.05))
save.image()
result4 = extrapolate.design(sr = 40, grange = 2, spar = seq(0, 1, by = 0.1), k = seq(0.05, 1, by = 0.05), duration = 30, freqs = seq(1,5, by = 1), amps = 5, noises = seq(0.01, 0.1, by = 0.05))
save.image()
result5 = extrapolate.design(sr = 40, grange = 2, spar = seq(0, 1, by = 0.1), k = seq(0.05, 1, by = 0.05), duration = 30, freqs = seq(1,5, by = 1), amps = 6, noises = seq(0.01, 0.1, by = 0.05))
save.image()
result6 = extrapolate.design(sr = 40, grange = 2, spar = seq(0, 1, by = 0.1), k = seq(0.05, 1, by = 0.05), duration = 30, freqs = seq(1,5, by = 1), amps = 7, noises = seq(0.01, 0.1, by = 0.05))
save.image()
result7 = extrapolate.design(sr = 40, grange = 2, spar = seq(0, 1, by = 0.1), k = seq(0.05, 1, by = 0.05), duration = 30, freqs = seq(1,5, by = 1), amps = 8, noises = seq(0.01, 0.1, by = 0.05))
save.image(file = "extrapolate.RData")
# stopCluster(cl)
final_result = rbind(result1, result2, result3, result4, result5, result6, result7)
mean_result = ddply(final_result, .(spar, k), summarize, ex_gain = round(mean(ex_gain / orig_err) * 100, digits = 1))
var_result = ddply(final_result, .(spar, k), summarize, ex_gain_var = var(ex_gain / orig_err))
mean_result['ex_gain_var'] = var_result['ex_gain_var']
sorted_result = mean_result[order(mean_result['ex_gain']),]
p = extrapolate.plot_optimization(mean_result)

ggsave(path = file.path("inst/figure/"), filename = "extrapolate_optimization_linear_2g_freq15_amp28.png", plot = p, scale = 1, width = 5, height = 2)

optimal_result = extrapolate.design(sr = 40, grange = 2, spar = 0.3, k = 1, duration = 30, freqs = seq(1,5, by = 1), amps = seq(2, 8, by = 0.5), noises = seq(0.01, 0.1, by = 0.05))
optimal_mean_result_over_noise = ddply(optimal_result, .(freq, amp, spar, k), summarize, ex_gain = round(mean(ex_gain / orig_err) * 100, digits = 1))
p = extrapolate.plot_distribution(optimal_mean_result_over_noise)
ggsave(path = file.path("inst/figure/"), filename = "extrapolate_optimal_spline_2g_0406_freq15_amp28.png", plot = p, scale = 1, width = 5, height = 2)
