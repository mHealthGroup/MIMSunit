#' Extrapolate input multi-channel accelerometer data
#'
#' \code{extrapolate} applies the extrapolation algorithm to a multi-channel
#' accelerometer data, trying to reconstruct the true movement from the
#' maxed-out samples.
#'
#' This function first linearly interpolates the input signal to 100Hz, and then
#' applies the extrapolation algorithm (see the manuscript) to recover the
#' maxed-out samples. Maxed-out samples are samples that are cut off because the
#' intensity of the underlying movement exceeds the dynamic range of the device.
#'
#' \code{extrapolate} processes a dataframe of a multi-channel accelerometer
#' signal. \code{extrapolate_single_col} processes a single-channel signal with
#' its timestamps and values specified in the first and second arguments.
#'
#' @section How is it used in MIMS-unit algorithm?: This function is the first
#'   step during MIMS-unit algorithm, applied before filtering.
#'
#' @param df dataframe. Input multi-channel accelerometer data. Used in
#'   \code{\link{extrapolate}}.  The first column should be the date/time
#' @param ... see following parameter list.
#' @param t POSIXct or numeric vector. Input index or timestamp sequence Used in
#'   \code{\link{extrapolate_single_col}}.
#' @param value numeric vector. Value vector used in
#'   \code{\link{extrapolate_single_col}}.
#' @param range numeric vector. The dynamic ranges of the input signal. Should
#'   be a 2-element numeric vector. \code{c(low, high)}, where \code{low} is
#'   the negative max value the device can reach and \code{high} is the positive
#'   max value the device can reach.
#' @param noise_level number. The tolerable noise level in \eqn{g} unit, should
#'   be between 0 and 1. Default is 0.03, which applies to most devices.
#' @param k number. Duration of neighborhood to be used in local spline
#'   regression for each side, in seconds. Default is 0.05, as optimized by
#'   MIMS-unit algorithm.
#' @param spar number. Between 0 and 1, to control how smooth we want to fit
#'   local spline regression, 0 is linear and 1 matches all local points.
#'   Default is 0.6, as optimized by MIMS-unit algorithm.
#' @return \code{extraplate} returns a dataframe with extrapolated multi-channel
#'   signal. \code{extrapolate_single_col} returns a dataframe with extrapolated
#'   single-channel signal, the timestamp col is in numeric values instead of
#'   POSIXct format.
#' @family extrapolation related functions
#' @name extrapolate
#' @examples
#'   # Use the maxed-out data for the conceptual diagram
#'   df = conceptual_diagram_data[
#'           conceptual_diagram_data['GRANGE'] == 4,
#'           c("HEADER_TIME_STAMP", "X")]
#'
#'   # Plot input
#'   illustrate_signal(df, range=c(-4, 4))
#'
#'   # Use the default parameter settings as in MIMunit algorithms
#'   # The dynamic range of the input data is -4g to 4g.
#'   output = extrapolate(df, range=c(-4, 4))
#'
#'   # Plot output
#'   illustrate_signal(output, range=c(-4, 4))
NULL

#' @rdname extrapolate
#' @export
extrapolate <- function(df, ...) {
  time_zone <- lubridate::tz(df[[1, 1]])
  t <- df[[1]]
  cn1 = colnames(df)[1]
  df <- df[2:ncol(df)]
  result <- plyr::alply(df,
                        .margins = 2, function(col_data) {
                          col_name <- names(col_data)[1]
                          output <- extrapolate_single_col(t, col_data[[1]], ...)
                          colnames(output) <- c(cn1, col_name)
                          return(output)
                        }
  )
  rm(df);
  result <- Reduce(
    function(x, y) {
      return(dplyr::inner_join(x, y, by = colnames(x)[1]))
    },
    result
  )
  names(result[2:ncol(result)]) <-
    paste("EXTRAPOLATED", names(result[2:ncol(result)]), sep = "_")
  result[, 1] <-
    as.POSIXct(result[, 1], origin = "1970-01-01", tz = time_zone)
  return(result)
}

#' @rdname extrapolate
#' @export
extrapolate_single_col <-
  function(t,
           value,
           range,
           noise_level = 0.03,
           k = 0.05,
           spar = 0.6) {
    # over sampling to 100Hz
    t_over <- seq(t[1], t[length(t)], by = 1 / 100)

    # over sampling to 100Hz with spline interpolation
    dat_over <-
      stats::spline(
        x = t,
        y = value,
        xout = t_over,
        method = "natural"
      )
    t1 = t[1]
    rm(t)
    rm(t_over)
    rm(value)
    dat_over <- data.frame(dat_over)
    if (lubridate::is.POSIXct(t1)) {
      dat_over[1] <- as.POSIXct(dat_over[[1]], origin = "1970-01-01")
    }
    if (is.unsorted(dat_over$x)) {
      dat_over <- dat_over[order(dat_over$x), ]
    }
    if (anyDuplicated(dat_over$x)) {
      not_dups <- !duplicated(dat_over$x)
      t <- dat_over$x[not_dups]
      value <- dat_over$y[not_dups]
      rm(not_dups)
    } else {
      t <- dat_over$x
      value <- dat_over$y
    }
    rm(dat_over); gc()

    # mark maxed out region using gamma distribution or threshold
    marker_fun <- .extrapolate_mark("gamma")
    marker <- marker_fun(t, value, range[1], range[2], noise_level)

    # mark neighbors
    neighbors <- .extrapolate_neighbor(marker, 100, k)

    # fit local spline regression
    points_ex <-
      .extrapolate_fit(t, value, neighbors, marker, spar, 100, k)
    rm(neighbors); gc()

    # interpolate with the original
    dat_interp <-
      .extrapolate_interpolate(t, value, marker, points_ex, 100)
    rm(points_ex); gc()
    return(dat_interp)
  }

.extrapolate_oversampling <- function(t, value) {
  time_zone <- lubridate::tz(t[1])
  t_over <- seq(t[1], t[length(t)], by = 1 / 100)

  # over sampling to 100Hz with spline interpolation
  dat_over <-
    stats::spline(
      x = t,
      y = value,
      xout = t_over,
      method = "natural"
    )
  t1 = t[1]
  rm(t_over)
  rm(t)
  rm(value)
  dat_over <- data.frame(dat_over)
  if (lubridate::is.POSIXct(t1)) {
    dat_over[1] <- as.POSIXct(dat_over[[1]], origin = "1970-01-01", tz = time_zone)
  }
  if (is.unsorted(dat_over$x)) {
    dat_over <- dat_over[order(dat_over$x), ]
  }
  if (anyDuplicated(dat_over$x)) {
    not_dups <- !duplicated(dat_over$x)
    t <- dat_over$x[not_dups]
    value <- dat_over$y[not_dups]
    rm(not_dups)
  } else {
    t <- dat_over$x
    value <- dat_over$y
  }
  rm(dat_over)

  return(list(t = t, value = value))
}

.extrapolate_mark <- function(method = "gamma") {
  return(switch(method, gamma = .mark_gamma, threshold = .mark_threshold))
}

.mark_gamma <-
  function(t, value, range_low, range_high, noise_sd = noise_sd) {
    # init the mark vector
    marker <- rep(0, length(t))

    # model of 3sd and shape para with confident probability at 0.95
    noise_sd <- noise_sd + 1e-05
    shape <- .optimize_gamma(3 * noise_sd)
    # mark using gamma distribution
    index = value >= 0
    marker[index] <-
      stats::pgamma(value[index] - (range_high - 5 * noise_sd),
                    shape = shape,
                    scale = 1
      )
    index = value < 0
    marker[index] <-
      -stats::pgamma(-value[index] + (range_low + 5 * noise_sd),
                     shape = shape,
                     scale = 1
      )
    return(marker)
  }

.optimize_gamma <- function(value) {
  i <- seq(0.5, 0.001, by = -0.001)
  result <- 0
  previous <- 1
  previous_ii <- 0
  for (ii in i)
  {
    current <- stats::pgamma(value, shape = ii, scale = 1)
    if (previous < 0.95 & current >= 0.95) {
      if (abs(0.95 - previous) > abs(current - 0.95)) {
        result <- ii
      } else {
        result <- previous_ii
      }
      break
    }
    previous <- current
    previous_ii <- ii
  }
  return(result)
}

.mark_threshold <-
  function(t, value, range_low, range_high, noise_sd = noise_sd) {
    # init the mark vector
    marker <- rep(0, length(t))

    # Compute the upper and lower threshold bound
    # according to the range and threshold
    u_bound <- range_high - 5 * noise_sd
    l_bound <- range_low + 5 * noise_sd

    marker[value >= u_bound] <- 1
    marker[value <= l_bound] <- -1
    return(marker)
  }

#' @importFrom magrittr %>%
.extrapolate_edges <- function(marker, confident, sr) {
  marker_diff_left <- c(0, diff(marker))
  marker_diff_right <- c(diff(marker), 0)

  threshold_maxedout <- sr * 5

  # hills
  positive_left_end <-
    which(marker_diff_left > confident & marker > 0)
  positive_right_start <-
    which(marker_diff_right < -confident & marker > 0)

  if (length(positive_left_end) - length(positive_right_start) == 1) {
    # end case > 2 second maxed out edge region
    if (length(marker) - positive_left_end[length(positive_left_end)] >
        threshold_maxedout) {
      positive_right_start <- c(positive_right_start, -1)
    } else {
      # < 2 second maxed out edge region, do nothing
      if (length(positive_left_end) == 1) {
        positive_left_end <- c()
      } else {
        positive_left_end <-
          positive_left_end[1:(length(positive_left_end) - 1)]
      }
    }
  } else if (length(positive_left_end) - length(positive_right_start == -1)) {
    # start case > 2 second maxed out edge region
    if (positive_right_start[1] > threshold_maxedout) {
      positive_left_end <- c(-1, positive_left_end)
    } else {
      # < 2 second maxed out edge region, do nothing
      if (length(positive_right_start) == 1) {
        positive_right_start <- c()
      } else {
        positive_right_start <-
          positive_right_start[2:length(positive_right_start)]
      }
    }
  }

  positive_left_end <- positive_left_end %>% stats::na.omit()
  positive_right_start <-
    positive_right_start %>% stats::na.omit()

  if (abs(length(positive_left_end) - length(positive_right_start)) > 2) {
    # something is wrong
    stop("The side of maxed out hills are diff > 1, should never happen")
  }

  if (any(positive_right_start - positive_left_end < 0) &&
      length(positive_right_start) > 1) {
    positive_left_end <- .shift(positive_left_end, 1)
    positive_right_start <- .shift(positive_right_start, -1)
  }

  positive_edges <-
    data.frame(
      left_end = positive_left_end,
      right_start = positive_right_start,
      stringsAsFactors = FALSE
    )
  rm(positive_left_end)
  rm(positive_right_start)

  # valleys
  negative_left_end <-
    which(marker_diff_left < -confident & marker < 0)
  negative_right_start <-
    which(marker_diff_right > confident & marker < 0)

  if (length(negative_left_end) - length(negative_right_start) == 1) {
    # end case > 2 second maxed out edge region
    if (length(marker) - negative_left_end[length(negative_left_end)] >
        threshold_maxedout) {
      negative_right_start <- c(negative_right_start, -1)
    } else {
      # < 2 second maxed out edge region, do nothing
      if (length(negative_left_end) == 1) {
        negative_left_end <- c()
      } else {
        negative_left_end <-
          negative_left_end[1:(length(negative_left_end) - 1)]
      }
    }
  } else if (length(negative_left_end) - length(negative_right_start == -1)) {
    # start case > 5 second maxed out edge region
    if (negative_right_start[1] > threshold_maxedout) {
      negative_left_end <- c(-1, negative_left_end)
    } else {
      # < 2 second maxed out edge region, do nothing
      if (length(negative_right_start) == 1) {
        negative_right_start <- c()
      } else {
        negative_right_start <-
          negative_right_start[2:length(negative_right_start)]
      }
    }
  }
  rm(marker)

  negative_left_end <- negative_left_end %>% stats::na.omit()
  negative_right_start <-
    negative_right_start %>% stats::na.omit()

  if (abs(length(negative_left_end) - length(negative_right_start)) > 2) {
    # something is wrong
    stop("The side of maxed out valleys are diff > 1, should never happen")
  }

  if (any(negative_right_start - negative_left_end < 0) &&
      length(negative_right_start) > 1) {
    negative_left_end <- .shift(negative_left_end, 1)
    negative_right_start <- .shift(negative_right_start, -1)
  }

  negative_edges <-
    data.frame(
      left_end = negative_left_end,
      right_start = negative_right_start,
      stringsAsFactors = FALSE
    )
  rm(negative_left_end)
  rm(negative_right_start)

  return(rbind(positive_edges, negative_edges))
}

#' @importFrom magrittr %>%
.extrapolate_neighbor <- function(marker, sr, k, confident = 0.5) {
  n_neighbor <- k * sr
  edges <- .extrapolate_edges(marker, confident, sr)
  if (nrow(edges) > 0) {
    neighbors <-
      edges %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        left_start = edges$left_end - n_neighbor + 1,
        right_end = edges$right_start + n_neighbor - 1
      ) %>%
      as.data.frame()
    rm(edges)
    neighbors$left_start <- pmax(neighbors$left_start, 1)
    neighbors$right_end <-
      pmin(neighbors$right_end, length(marker))
    neighbors$left_start[neighbors$left_end == -1] <- -1
    neighbors$right_end[neighbors$right_start == -1] <- -1
  } else {
    rm(edges)
    neighbors <-
      data.frame(
        left_start = c(),
        right_end = c(),
        left_end = c(),
        right_start = c()
      )
  }
  return(neighbors)
}

.shift <- function(v, lag, truncate = TRUE) {
  n <- length(v)
  if (lag > 0) {
    v <- v[-((n - lag + 1):n)]
  } else {
    v <- v[-(1:(-lag))]
  }
  return(v)
}

#' @importFrom magrittr %>%
.extrapolate_fitline <-
  function(t,
           value,
           neighbors,
           marker,
           spar,
           sr,
           k,
           model = "spline") {
    neighbors$index <- 1:nrow(neighbors)
    point_ex <- neighbors %>% plyr::adply(1, function(neighbor) {
      # validate neighboring
      fitted_left <-
        .fit_weighted(
          t,
          value,
          marker,
          neighbor$left_start,
          neighbor$left_end,
          spar,
          sr,
          k,
          model
        )
      fitted_right <-
        .fit_weighted(
          t,
          value,
          marker,
          neighbor$right_start,
          neighbor$right_end,
          spar,
          sr,
          k,
          model
        )

      st <- t[neighbor$left_end]
      left_end <- 0
      right_start <-
        as.numeric(t[neighbor$right_start] - st, units = "secs")

      middle_t <- (left_end + right_start) / 2
      middle_t <- st + middle_t
      left_x_ex <- c(seq(st, middle_t, 1 / 100), middle_t)
      right_x_ex <-
        c(middle_t, seq(middle_t, st + right_start, 1 / 100))
      # if (left_end == right_start) {
      #   return(data.frame(
      #     t_ex = c(),
      #     value_ex = c(),
      #     type_ex = c(),
      #     index =
      #   ))
      # }
      switch(model,
             spline = {
               left_ex <- fitted_left %>% stats::predict(x = as.numeric(left_x_ex))
               right_ex <-
                 fitted_right %>% stats::predict(x = as.numeric(right_x_ex))
               type_ex <- rep("left_line", length(left_ex$y))

               point_ex_left <-
                 fitted_left %>% stats::predict(x = as.numeric(middle_t))
               point_ex_right <-
                 fitted_right %>% stats::predict(x = as.numeric(middle_t))
               point_ex <- (point_ex_left$y + point_ex_right$y) / 2
               type_ex <- c(type_ex, "point")
               type_ex <-
                 c(type_ex, rep("right_line", length(right_ex$y)))
               index <- rep(neighbor$index, length(type_ex))
             }
      )
      return(data.frame(
        t_ex = c(left_x_ex, middle_t, right_x_ex),
        value_ex = c(left_ex$y, point_ex, right_ex$y),
        type_ex = type_ex,
        index = index
      ))
    },
    .inform = TRUE, .id = NULL, .expand = FALSE
    )
    return(point_ex)
  }

#' @importFrom magrittr %>%
.extrapolate_fit <-
  function(t,
           value,
           neighbors,
           marker,
           spar,
           sr,
           k,
           model = "spline") {
    point_ex <- neighbors %>% plyr::adply(1, function(neighbor) {
      # validate neighboring

      fitted_left <-
        .fit_weighted(
          t,
          value,
          marker,
          neighbor$left_start,
          neighbor$left_end,
          spar,
          sr,
          k,
          model
        )
      fitted_right <-
        .fit_weighted(
          t,
          value,
          marker,
          neighbor$right_start,
          neighbor$right_end,
          spar,
          sr,
          k,
          model
        )

      if (is.null(fitted_left)) {
        n_rep <- neighbor$right_start - 1
        point_ex <- rep(-200, n_rep) # give an extra huge value
        middle_t <- t[1:(neighbor$right_start - 1)]
      } else if (is.null(fitted_right)) {
        n_rep <- length(t) - neighbor$left_end
        point_ex <- rep(-200, n_rep) # give an extra huge value
        middle_t <- t[(neighbor$left_end + 1):length(t)]
      } else {
        st <- t[neighbor$left_end]
        left_end <- 0
        right_start <-
          as.numeric(t[neighbor$right_start] - st, units = "secs")

        middle_t <- (left_end + right_start) / 2
        middle_t <- st + middle_t
        switch(model, linear = {
          left_ex <-
            fitted_left %>%
            stats::predict(data.frame(over_t = as.numeric(middle_t)))
          right_ex <-
            fitted_right %>%
            stats::predict(data.frame(over_t = as.numeric(middle_t)))
          point_ex <- (left_ex + right_ex) / 2
        },
        spline = {
          left_ex <- fitted_left %>% stats::predict(x = as.numeric(middle_t))
          right_ex <-
            fitted_right %>% stats::predict(x = as.numeric(middle_t))
          point_ex <- (left_ex$y + right_ex$y) / 2
        }
        )
      }
      return(data.frame(t_ex = middle_t, value_ex = point_ex))
    },
    .inform = TRUE,
    .id = NULL,
    .expand = FALSE
    )
    return(point_ex)
  }

.fit <-
  function(t,
           value,
           marker,
           start,
           end,
           spar,
           sr,
           k,
           model = "spline") {
    if (start < 0) {
      start <- 1
    }
    if (end > length(t)) {
      end <- length(t)
    }
    # over sampling so that we have enough points
    n_over <- k * sr
    index = start:end
    sub_t <- t[index]
    sub_value <- value[index]
    sp <- stats::approx(x = sub_t, y = sub_value, n = n_over)
    over_t <- sp$x[-n_over]
    over_value <- sp$y[-n_over]

    # fit locally with spline smoothing
    fitted <-
      switch(
        model,
        spline = stats::smooth.spline(over_t, over_value, spar = spar),
        linear = stats::lm(over_value ~ over_t, na.action = stats::na.omit)
      )
    return(fitted)
  }

.fit_weighted <-
  function(t,
           value,
           marker,
           start,
           end,
           spar,
           sr,
           k,
           model = "spline") {
    # if(start < 0) start = 1 if(end > length(t)) end = length(t)

    # over sampling so that we have enough points
    n_over <- k * sr

    if (start == -1 && end == -1) {
      # start edge case
      fitted <- NULL
    } else {
      index = start:end
      sub_t <- t[index]
      sub_value <- value[index]
      weight <- 1 - marker[index]
      sp <- stats::approx(x = sub_t, y = sub_value, n = n_over)
      rm(sub_value)
      weight <- stats::approx(x = sub_t, y = weight, n = n_over)
      rm(sub_t)
      over_t <- sp$x
      over_value <- sp$y
      rm(sp)
      weight <- weight$y
      # fit locally with spline smoothing
      fitted <-
        switch(
          model,
          spline = stats::smooth.spline(over_t,
                                        over_value,
                                        weight,
                                        spar = spar
          ),
          linear = stats::lm(
            over_value ~ over_t,
            weights = weight,
            na.action = stats::na.omit
          )
        )
    }
    return(fitted)
  }

#' @importFrom magrittr %>%
.extrapolate_interpolate <-
  function(t, value, marker, points_ex, sr, confident = 0.5) {

    first_t = dplyr::first(t)
    last_t = dplyr::last(t)

    mark_it <- abs(marker) < confident
    rm(marker); gc()
    length_t_mark = sum(mark_it, na.rm = TRUE)

    # t_mark = t value_mark = value
    if (length_t_mark / length(t) < 0.3) {
      dat <- data.frame(t = t, value = value)
    } else {
      # only have to calculate these if the above isn't true
      # since mark_it is logical, can just do the sum above
      value_mark <- value[mark_it]
      rm(value)

      t_mark <- t[mark_it]
      rm(mark_it)

      rm(t); gc()

      dat <-
        rbind(
          data.frame(t = t_mark, value = value_mark),
          data.frame(t = points_ex$t_ex, value = points_ex$value_ex)
        ) %>% dplyr::arrange(t)
      rm(t_mark); gc()
      rm(value_mark); gc()
    }
    t = dat$t
    value = dat$value
    rm(dat); gc()
    t_interp <-
      seq(first_t, last_t, by = 1 / sr)
    dat <-
      stats::spline(t, y = value, xout = t_interp) %>%
      as.data.frame() %>%
      stats::na.omit()
    names(dat) <- c("t", "value")

    return(dat)
  }
