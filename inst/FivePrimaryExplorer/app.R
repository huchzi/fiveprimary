#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(xlsx)
library(jsonlite)
library(ggplot2)
library(data.table)

gvars <- reactiveValues()

read_fiveprimaryJSON <- function(path) {
  read_json(path, simplifyVector = TRUE)$data
}

extract_thresholds <- function(result) {
  data.table(preset = result$preset$presetName,
             frequency = result$preset$modulation$frequencyRed,
             strategy = ifelse(result$preset$strategy$testContrast,
                               "contrast", "frequency"),
             finished = result$finished,
             threshold = result$value)
}

extract_presets <- function(result) {
  data.table(result$preset[, c("presetName", "id", "description", "receptorType")],
             result$preset$modulation,
             result$preset$background,
             result$preset$fixPoint,
             result$preset$geometry,
             result$preset$geometryModulation,
             result$preset$strategy
        )
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Five primary explorer"),

    sidebarLayout(
        sidebarPanel(
            fileInput("readJSON", "Load JSON file"),
            downloadButton("dowloadExcel", "Download Excel File")
        ),

        mainPanel(
          plotOutput("thresholds")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$thresholds <- renderPlot({
    file <- input$readJSON

    req(file)
    gvars$filename <- file$name
    gvars$data <- read_fiveprimaryJSON(file$datapath)
    gvars$results <- extract_thresholds(gvars$data)

    ggplot(gvars$results, aes(x = frequency, y = 1 / threshold)) +
      geom_line() +
      scale_x_log10("Temporal frequency [Hz]") +
      scale_y_log10("Sensitivity") +
      theme_bw()
  })

  output$dowloadExcel <- downloadHandler(
    filename = function() {gsub(".json", ".xlsx", gvars$filename)},
    content = function(newFile) {
      req(gvars$results)
      wb <- createWorkbook()
      sheet1 <- createSheet(wb, "Thresholds")
      addDataFrame(gvars$results, sheet = sheet1)
      sheet2 <- createSheet(wb, "Stimulus")
      addDataFrame(extract_presets(gvars$data), sheet = sheet2)
      saveWorkbook(wb, newFile)
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)
