#' Plot given raw signal in illustration diagram style.
#'
#' \code{illustrate_signal} plots the given uniaxial signal in illustration diagram style.
#'   Illustration diagram style hides axes markers, unnecessary guidelines.
#'
#' @param data data.frame. The input uniaxial signal. First column should be timestamp.
#' @param point_size number. The size of the plotted data point.
#' @param plot_point Bool. Plot signal as points if TRUE.
#' @param line_size number. The line width of the plotted signal curve.
#' @param plot_line Bool. Plot signal with curve if TRUE.
#' @param range vector. Dynamic range of the signal.
#' @param plot_maxed_out_line Bool. Plot dynamic range lines if TRUE. Dynamic range is set by `range`.
#' @param plot_origin Bool. Plot the 0 horizontal line if TRUE.
#' @param title Char. The title of the plot.
#' @param plot_title Bool. Plot title if TRUE.
#'
#' @return ggplot2 graph object. The graph to be shown.
#' @family visualization functions.
#' @export
#' @examples
#'   # Use sample data for testing
#'   df = sample_raw_accel_data
#'
#'   # Plot it with default settings
#'   illustrate_signal(df)
#'
#'   # Plot with a different style
#'   illustrate_signal(df, point_size=1, line_size=1)
#'
#'   # Turn off annotation lines
#'   illustrate_signal(df, plot_maxed_out_line = FALSE, plot_origin = FALSE)
#'
#'   # Use title
#'   illustrate_signal(df, plot_title=TRUE, title = "This is a title")
illustrate_signal <- function(data,
                              point_size = 0.3,
                              plot_point = TRUE,
                              line_size = 0.3,
                              plot_line = TRUE,
                              range = c(-2, 2),
                              plot_maxed_out_line = TRUE,
                              plot_origin = TRUE,
                              title = NULL,
                              plot_title = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  colnames(data) <- c("HEADER_TIME_STAMP", "value")
  p <- ggplot2::ggplot(
    data = data,
    ggplot2::aes_string(x = 'HEADER_TIME_STAMP', y = 'value')
  )
  if (plot_line) {
    p <- p + ggplot2::geom_line(size = line_size)
  }
  if (plot_point) {
    p <- p + ggplot2::geom_point(size = point_size)
  }
  if (plot_maxed_out_line) {
    p <- p + ggplot2::geom_hline(
      yintercept = range[1],
      size = 0.3,
      linetype = "dashed"
    )
    p <- p + ggplot2::geom_hline(
      yintercept = range[2],
      size = 0.3,
      linetype = "dashed"
    )
  }
  if (plot_origin) {
    p <- p + ggplot2::geom_hline(yintercept = 0, size = 0.1)
  }
  p <- p + ggplot2::theme_bw(base_size = 12) + ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    plot.margin = ggplot2::unit(c(0.1, 0.1, -0.32, -0.32), "line")
  ) + ggplot2::ylim(-8.1, 8.1) + ggplot2::labs(x = NULL, y = NULL, title = title)
  return(p)
}


#' Plot illustrations about extrapolation in illustration style.
#'
#' \code{illustrate_extrapolation} plots elements of extrapolations
#' (e.g., marked points, reference lines) in the same style as
#' \code{\link{illustrate_signal}}.
#'
#' @param df data.frame.The original data before extrapolation.
#' @param dynamic_range numerical vector. The dynamic ranges of the input
#'   signal. Should be a 2-element numerical vector. \code{c(low, high)}, where
#'   \code{low} is the negative max value the device can reach and \code{high}
#'   is the positive max value the device can reach.
#' @param title Char. The title of the plot.
#' @param show_neighbors bool. Show the points used for extrapolation if TRUE.
#' @param show_extrapolated_points_and_lines bool. Show the extrapolated points
#'  and curves used for extrapolation.
#' @param ... Parameters that can be used to tune extrapolation, including
#'   \code{spar}, \code{k}, and \code{noise_level}. See \code{\link{extrapolate}}
#'   for explanations.
#'
#' @return ggplot2 graph object. The graph to be shown.
#' @family visualization functions.
#' @export
#' @examples
#'   # Use the maxed-out data for the conceptual diagram
#'   df = conceptual_diagram_data[
#'          conceptual_diagram_data['GRANGE'] == 2,
#'          c("HEADER_TIME_STAMP", "X")]
#'
#'   # Plot extrapolation illustration using default settings
#'   illustrate_extrapolation(df, dynamic_range=c(-2,2))
#'
#'   # Do not show neighbor points
#'   illustrate_extrapolation(df, dynamic_range=c(-2,2), show_neighbors=FALSE)
#'
#'   # Do not show extrapolated points and lines
#'   illustrate_extrapolation(df,
#'                            dynamic_range=c(-2,2),
#'                            show_extrapolated_points_and_lines=FALSE)
illustrate_extrapolation <-
  function(df,
           dynamic_range,
           title = NULL,
           show_neighbors = TRUE,
           show_extrapolated_points_and_lines = TRUE, ...) {
    results = .prepare_for_extrapolation_illustration(df, dynamic_range = dynamic_range, ...)
    if(is.null(results$between_neighbor_df)) {
      p <- illustrate_signal(
        df,
        point_size = 1,
        plot_line = FALSE,
        title = title,
        range = dynamic_range,
        plot_title = TRUE
      )
      return(p)
    } else {
      p <- illustrate_signal(
        results$between_neighbor_df[, 1:2],
        point_size = 1,
        plot_line = FALSE,
        title = title,
        range = dynamic_range,
        plot_title = TRUE
      )
      if (show_neighbors) {
        if (!is.null(results$left_neighbors_df)) {
          p <- p +
            ggplot2::geom_point(
              data = results$left_neighbors_df,
              ggplot2::aes_string(x = 'HEADER_TIME_STAMP', y = 'VALUE'),
              shape = 1,
              size = 1
            )
        }
        if (!is.null(results$right_neighbors_df)) {
          p <- p +
            ggplot2::geom_point(
              data = results$right_neighbors_df,
              ggplot2::aes_string(x = 'HEADER_TIME_STAMP', y = 'VALUE'),
              shape = 1,
              size = 1
            )
        }
      }
      if (show_extrapolated_points_and_lines) {
        if (!is.null(results$points_ex_df)) {
          p <- p +
            ggplot2::geom_point(
              data = results$points_ex_df,
              ggplot2::aes_string(x = 'HEADER_TIME_STAMP', y = 'VALUE'),
              shape = 17,
              size = 2
            ) +
            ggplot2::geom_vline(
              xintercept = as.numeric(results$points_ex_df[, 1]),
              size = 0.3,
              linetype = "dotted"
            )
        }

        if (!is.null(results$fitted_line_df)) {
          indices <- unique(results$fitted_line_df$index)
          for (i in indices) {
            left_fit_line_df <- results$fitted_line_df[results$fitted_line_df$index == i &
                                                         results$fitted_line_df$type == "left_line", c(1, 2)]
            right_fit_line_df <- results$fitted_line_df[results$fitted_line_df$index == i &
                                                          results$fitted_line_df$type == "right_line", c(1, 2)]
            colnames(left_fit_line_df) <- c("HEADER_TIME_STAMP", "value")
            colnames(right_fit_line_df) <- c("HEADER_TIME_STAMP", "value")
            p <- p + ggplot2::geom_line(
              data = left_fit_line_df,
              ggplot2::aes_string(x = "HEADER_TIME_STAMP", y = "value"),
              size = 0.5
            ) +
              ggplot2::geom_line(
                data = right_fit_line_df,
                ggplot2::aes_string(x = "HEADER_TIME_STAMP", y = "value"),
                size = 0.5
              )
          }
        }
      }
      return(p)
    }
  }

.prepare_for_extrapolation_illustration = function(df, dynamic_range, noise_level=0.03, k=0.05, spar=0.6) {
  df = df[,c(1,2)]
  df = data.frame(.extrapolate_oversampling(df[[1]], df[[2]]))
  colnames(df) = c('HEADER_TIME_STAMP', 'VALUE')
  markers = .extrapolate_mark("gamma")(df$HEADER_TIME_STAMP,
                                       df$VALUE,
                                       dynamic_range[1],
                                       dynamic_range[2],
                                       noise_level)
  markers_df = data.frame(HEADER_TIME_STAMP = df$HEADER_TIME_STAMP,
                          VALUE = abs(markers))
  neighbors = .extrapolate_neighbor(markers, 100, k)
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
    fitted_line_df = .extrapolate_fitline(
      df[[1]],
      df[[2]],
      neighbors,
      markers,
      spar,
      100,
      k
    )
    colnames(fitted_line_df) = c('HEADER_TIME_STAMP', 'VALUE', 'type', 'index')
    left_neighbors_df = df[left_indices, ]
    right_neighbors_df = df[right_indices, ]
    between_neighbor_df = df[c(-left_indices, -right_indices), ]
    between_neighbor_without_maxed_out_df = df[c(-left_indices, -right_indices, -mo_indices), ]
    maxed_out_df = df[mo_indices, ]
    maxed_out_df$weight = markers_df[mo_indices, 'value']
    points_ex_df = fitted_line_df[fitted_line_df$type == 'point', c(1, 2)]
  } else {
    marks_df = NULL
    left_neighbors_df = NULL
    right_neighbors_df = NULL
    maxed_out_df = NULL
    between_neighbor_df = NULL
    between_neighbor_without_maxed_out_df = NULL
    fitted_line_df = NULL
    points_ex_df = NULL
  }
  return(
    list(
      df = df,
      between_neighbor_df = between_neighbor_df,
      left_neighbors_df = left_neighbors_df,
      right_neighbors_df = right_neighbors_df,
      maxed_out_df = maxed_out_df,
      between_neighbor_without_maxed_out_df = between_neighbor_without_maxed_out_df,
      markers_df = markers_df,
      fitted_line_df = fitted_line_df,
      points_ex_df = points_ex_df
    )
  )
}


#' Plot MIMS unit values or raw signal using dygraphs interactive plotting library.
#'
#' \code{generate_interactive_plot} plots MIMS unit values or raw signal
#'   using dygraphs interactive plotting library.
#'
#' @param df data.frame.The dataframe storing MIMS unit values or raw
#'   accelerometer signal. The first column should be timestamps.
#' @param y_label str. The label name to be put on the y axis.
#' @param value_cols numerical vector. The indices of columns storing values,
#'   typically starting from the second column. The default is `c(2,3,4)`.
#'
#' @importFrom magrittr %>%
#' @return A dygraphs graph object. When showing, the graph will be plotted in
#'   a html widgets in an opened browser.
#' @family visualization functions.
#' @export
#' @examples
#'   # Use sample data for testing
#'   df = sample_raw_accel_data
#'
#'   # Plot using default settings, due to pkgdown limitation, no interactive
#'   # plots will be shown on the website page.
#'   generate_interactive_plot(df,
#'                             y_label="Acceleration (g)")
#'
#'   # The function can be used to plot MIMS unit values as well
#'   mims = mims_unit(df, dynamic_range=c(-8, 8))
#'   generate_interactive_plot(mims,
#'                             y_label="MIMS-unit values",
#'                             value_cols=c(2))
generate_interactive_plot = function(df, y_label, value_cols=c(2,3,4)) {
  xts_values = xts::xts(df[,value_cols], df[,1])
  range = max(xts_values) - min(xts_values)
  min_y = min(xts_values) - range * 1/2
  max_y = max(xts_values) + range * 1/2
  g = dygraphs::dygraph(xts_values) %>%
    dygraphs::dyRangeSelector() %>%
    dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(4, "Set2")) %>%
    dygraphs::dyLegend(width = 400) %>%
    dygraphs::dyAxis("y", valueRange = c(min_y, max_y), label = y_label) %>%
    dygraphs::dyAxis("x", label = 'Time')
  return(g)
}