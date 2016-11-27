rm(list = ls(all.names = TRUE))
require(plyr)
require(dplyr)
require(reshape2)
require(ggplot2)
require(signal)
require(Counts)
require(caTools)
optimize_extrapolation_parameters = function(test_set) {

  k = seq(from = 0.05, to = 0.95, by = 0.1)
  spar = seq(from = 0, to = 1, by = 0.1)

  configs = expand.grid(k = k, spar = spar)
  result = adply(configs, 1, function(config){
    k = config$k
    spar = config$spar
    print(sprintf("process: k = %.2f, spar = %.2f", k, spar))
    result_for_config = ddply(test_set, c("freq", "amp"), function(test_case) {
      freq = test_case$freq[1]
      amp = test_case$amp[1]
      sr = test_case$sr[1]
      grange = test_case$grange[1]

      extrapolated_signal = extrapolate(
        t = test_case$ts,
        value = test_case$test_signal,
        range = c(-grange, grange),
        noise_std = 0.05,
        k = k,
        spar = spar
      )
      test_signal = spline(test_case$ts, test_case$test_signal, xout = extrapolated_signal$t)
      true_signal = spline(test_case$ts, test_case$true_signal, xout = extrapolated_signal$t)
      extrapolated_case = data.frame(ts = extrapolated_signal$t, extrapolated_signal = extrapolated_signal$value, test_signal = test_signal$y, true_signal = true_signal$y)
      # exclude the heading and tailing small segments (impact of step response)
      extrapolated_case = extrapolated_case %>% subset(ts > 0.5 & ts < ts[length(ts)] - 0.5)

      true_auc = trapz(extrapolated_case$ts, abs(extrapolated_case$true_signal))
      test_auc = trapz(extrapolated_case$ts, abs(extrapolated_case$test_signal))
      extrapolated_auc = trapz(extrapolated_case$ts, abs(extrapolated_case$extrapolated_signal))
      test_err = (true_auc - test_auc) / true_auc
      extrapolated_err = (true_auc - extrapolated_auc) / true_auc
      extrapolated_rate = (test_err - abs(extrapolated_err)) / test_err
      extrapolated_case = data.frame(
                                     freq = freq,
                                     amp = amp,
                                     sr = sr,
                                     grange = grange,
                                     test_err = test_err,
                                     extrapolated_err = extrapolated_err,
                                     extrapolated_rate = extrapolated_rate)
      return(extrapolated_case)
    }, .progress = progress_time(), .inform = TRUE)
    result_for_config = data.frame(result_for_config, k = k, spar = spar)
    return(result_for_config)
  }, .progress = progress_time(), .inform = TRUE)
  return(result)
}

generate_test_set_on_virtual_device = function(sr,
                                               grange,
                                               duration = 60,
                                               random = 1) {
  amps = 3:8
  freqs = 1:8

  configs = expand.grid(freq = freqs, amp = amps)
  test_set = adply(configs, 1, function(config) {
    freq = config$freq
    amp = config$amp
    x = seq(0, duration, length = duration * sr)
    set.seed(random)
    noise = rnorm(length(x), sd = 0.05)
    phase = runif(1, min = 0, max = 2 * pi)
    true_signal = amp * sin(2 * pi * freq * x + phase) + noise
    test_signal = amp * sin(2 * pi * freq * x + phase) + noise
    test_signal[test_signal >= grange] = grange + noise[test_signal >= grange]
    test_signal[test_signal <= -grange] = -grange + noise[test_signal <= -grange]
    result = data.frame(
      ts = x,
      test_signal = test_signal,
      true_signal = true_signal,
      freq = freq,
      amp = amp,
      sr = sr,
      grange = grange,
      phase = phase,
      random = random
    )
    return(result)
  }, .progress = progress_time())

  return(test_set)
}

test_set = generate_test_set_on_virtual_device(sr = 40,
                                               grange = 2,
                                               duration = 60)

test_result = optimize_extrapolation_parameters(test_set)

saveRDS(test_result, file = "inst/table/extrapolate/optimize_on_40Hz_2g.rds", compress = 'gzip')