#' Plot given raw signal in illustration diagram style.
#'
#' \code{illustrate_signal} plots the given uniaxial signal in illustration diagram style.
#' Illustration diagram style hides axes markers, uncessary guidelines.
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
#'
illustrate_signal <- function(data,
                              point_size = 0.3,
                              plot_point = T,
                              line_size = 0.3,
                              plot_line = T,
                              range = c(-2, 2),
                              plot_maxed_out_line = T,
                              plot_origin = T,
                              title = NULL,
                              plot_title = T) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  colnames(data) = c('HEADER_TIME_STAMP', 'value')
  p = ggplot2::ggplot(data = data,
                      ggplot2::aes(x = HEADER_TIME_STAMP, y = value))
  if (plot_line) {
    p = p + ggplot2::geom_line(size = line_size)
  }
  if (plot_point) {
    p = p + ggplot2::geom_point(size = point_size)
  }
  if (plot_maxed_out_line) {
    p = p + ggplot2::geom_hline(yintercept = range[1],
                                size = 0.3,
                                linetype = 'dashed')
    p = p + ggplot2::geom_hline(yintercept = range[2],
                                size = 0.3,
                                linetype = 'dashed')
  }
  if (plot_origin) {
    p = p + ggplot2::geom_hline(yintercept = 0, size = 0.1)
  }
  p = p + ggplot2::theme_bw(base_size = 12) + ggplot2::theme(
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
#' @param title Char. The title of the plot.
#' @param between_neighbor_df data.frame. Dataframe containing points between the points used for extrapolation.
#' @param left_neighbors_df data.frame. Dataframe containing the points of left side used for extrapolation.
#' @param right_neighbors_df data.frame. Dataframe containing the points of right side used for extrapolation.
#' @param show_neighbors bool. Show the points used for extrapolation if TRUE.
#' @param extrap_points_df data.frame. Dataframe containing the points got from extrapolation.
#' @param fitted_line_df data.frame. Dataframe containing the spline curves used during extrapolation.
#' @param show_extrapolated_points_and_lines bool. Show the extrapolated points and curves used for extrapolation.
#'
#' @return ggplot2 graph object. The graph to be shown.
#' @family visualization functions.
#' @export
#'
illustrate_extrapolation <-
  function(df,
           between_neighbor_df = NULL,
           title = NULL,
           left_neighbors_df = NULL,
           right_neighbors_df = NULL,
           show_neighbors = T,
           extrap_points_df = NULL,
           fitted_line_df = NULL,
           show_extrapolated_points_and_lines = T) {
    if (is.null(between_neighbor_df)) {
      p = MIMSunit::illustrate_signal(
        df[, 1:2],
        point_size = 1,
        plot_line = F,
        title = title,
        range = c(-df$GRANGE[1], df$GRANGE[1]),
        plot_title = T
      )
    } else {
      p = MIMSunit::illustrate_signal(
        between_neighbor_df[, 1:2],
        point_size = 1,
        plot_line = F,
        title = title,
        range = c(
          -between_neighbor_df$GRANGE[1],
          between_neighbor_df$GRANGE[1]
        ),
        plot_title = T
      )
      if (show_neighbors) {
        if (!is.null(left_neighbors_df)) {
          p = p +
            ggplot2::geom_point(
              data = left_neighbors_df,
              ggplot2::aes(x = left_neighbors_df[, 1], y = left_neighbors_df[, 2]),
              shape = 1,
              size = 1
            )
        }
        if (!is.null(right_neighbors_df)) {
          p = p +
            ggplot2::geom_point(
              data = right_neighbors_df,
              ggplot2::aes(x = right_neighbors_df[, 1], y = right_neighbors_df[, 2]),
              shape = 1,
              size = 1
            )
        }
      }
      if (show_extrapolated_points_and_lines) {
        if (!is.null(extrap_points_df)) {
          p = p +
            ggplot2::geom_point(
              data = extrap_points_df,
              ggplot2::aes(x = extrap_points_df[, 1], y = extrap_points_df[, 2]),
              shape = 17,
              size = 2
            ) +
            ggplot2::geom_vline(
              xintercept = as.numeric(extrap_points_df[, 1]),
              size = 0.3,
              linetype = 'dotted'
            )
        }

        if (!is.null(fitted_line_df)) {
          indices = unique(fitted_line_df$index)
          for (i in indices) {
            left_fit_line_df = fitted_line_df[fitted_line_df$index == i &
                                                fitted_line_df$type == 'left_line', c(1, 2)]
            right_fit_line_df = fitted_line_df[fitted_line_df$index == i &
                                                 fitted_line_df$type == 'right_line', c(1, 2)]
            colnames(left_fit_line_df) = c('HEADER_TIME_STAMP', 'value')
            colnames(right_fit_line_df) = c('HEADER_TIME_STAMP', 'value')
            p = p + ggplot2::geom_line(
              data = left_fit_line_df,
              ggplot2::aes(x = HEADER_TIME_STAMP, y = value),
              size = 0.5
            ) +
              ggplot2::geom_line(
                data = right_fit_line_df,
                ggplot2::aes(x = HEADER_TIME_STAMP, y = value),
                size = 0.5
              )
          }
        }
      }
    }
    return(p)
  }