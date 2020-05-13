#' Run shiny app to compute MIMSunit values from files
#'
#' \code{shiny_app} runs a local shiny app that provides a user friendly interface
#' to compute mims unit values from files stored in mhealth or actigraph format.
#'
#' @section How is it used in MIMS-unit algorithm?: This provides a user friendly
#' graphical interface to load local files, call \code{\link{mims_unit_from_files}}
#' and display the results as an interactive graph.
#'
#' @family Top level API functions
#' @export
#' @examples
#'   \dontrun{
#'   shiny_app()
#'   }
shiny_app <- function() {
  shiny::runApp(appDir = system.file("app", package='MIMSunit'))
}