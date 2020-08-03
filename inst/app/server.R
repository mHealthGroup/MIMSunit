#' This is an example Shiny application
#' It is the same as example_02 in the RStudio Shiny Tutorial

library(shiny)
library(dygraphs)
library(xts)

options(shiny.maxRequestSize=50*1024^2) # Limit to upload data size to 50 MB

server <- function(input, output) {

  mims_unit_values <- eventReactive(input$compute, {
    file_paths = input$input_files$datapath
    results = c()
    if (length(file_paths) == 1){
      input_files = c(file_paths)
      results = MIMSunit::mims_unit_from_files(
        c(input_files),
        epoch = paste(input$epoch, 'sec'),
        dynamic_range = c(-input$dynamic_range, input$dynamic_range),
        file_type = input$file_type,
        output_mims_per_axis = TRUE, header=input$header, has_ts=input$timestamps)
    } else if (length(file_paths) > 1 && !input$independent) {
      input_files = file_paths
      results = MIMSunit::mims_unit_from_files(
        c(input_files),
        epoch = paste(input$epoch, 'sec'),
        dynamic_range = c(-input$dynamic_range, input$dynamic_range),
        file_type = input$file_type,
        output_mims_per_axis = TRUE, header=input$header, has_ts=input$timestamps)
    } else if (length(file_paths) > 1 && input$independent) {
      for (file_path in file_paths) {
        result = MIMSunit::mims_unit_from_files(
          c(file_path),
          epoch = paste(input$epoch, 'sec'),
          dynamic_range = c(-input$dynamic_range, input$dynamic_range),
          file_type = input$file_type,
          output_mims_per_axis = TRUE, header=input$header, has_ts=input$timestamps)
      }
      results = append(results, result)
    }
    return(results)
  })

  output$dygraph <- renderDygraph({
    results = mims_unit_values()
    mims_xts = xts(results[,2:5], results[,1])
    dygraph(mims_xts) %>% dyRangeSelector() %>% dyOptions(colors = RColorBrewer::brewer.pal(4, "Set2")) %>% dyLegend(width = 400) %>% dyAxis("y", valueRange = c(0, 2 * max(mims_xts)))
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste('mims_unit_values_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(mims_unit_values(), con, row.names = FALSE, quote = FALSE)
    }
  )
}