#' This is an example shiny application
#' It is the same as example_02 in the RStudio Shiny Tutorial

library(shiny)
library(dygraphs)

ui <- fluidPage(

  # Application title
  titlePanel("Compute MIMS-unit values"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("input_files", "Choose CSV Files",
                multiple = TRUE,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      selectInput("file_type", "File type:",
                  c("mHealth" = "mhealth",
                    "Actigraph" = "actigraph"
                  )
      ),
      conditionalPanel(
        condition = "input.input_files && input.input_files.length > 1",
        checkboxInput("independent", "Process each file independently", TRUE)
      ),
      conditionalPanel(
        condition = "input.file_type == 'actigraph'",
        checkboxInput("header", "Header on the first row", TRUE),
        checkboxInput("timestamps", "Timestamps on the first column", TRUE)
      ),
      sliderInput("epoch",
                  "Epoch (secs)",
                  min = 1,
                  max = 300,
                  value = 5,
                  step = 1
      ),
      sliderInput("dynamic_range",
                  "Dynamic range (g):",
                  min = 1,
                  max = 20,
                  value = 8,
                  step = 1),
      actionButton("compute", "Compute MIMS-unit values")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      dygraphOutput("dygraph"),
      conditionalPanel(
        condition = "output.dygraph",
        downloadButton("download_data", "Download")
      )
    )
  )
)
