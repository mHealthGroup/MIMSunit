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
  p = p + theme_bw(base_size = 12) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(0.1, 0.1,-0.32,-0.32), "line")
  ) + ylim(-5.5, 5.5) + labs(x = NULL, y = NULL, title = title)
  return(p)
}