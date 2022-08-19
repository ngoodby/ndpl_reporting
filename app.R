library(shiny)
library(tidyverse)
library(here)

ui <- fluidPage(
  tags$head(tags$style(".shiny-notification {position: fixed; top: 25%; left: 25%; font-size: 30px}")),
  titlePanel("NABat Hub Landowner Report Generator"),
  textInput("enter_username", "NABat Username", value = "",
            placeholder = "Enter NABat username here"),
  textInput("enter_password", "NABat Password", value = "",
            placeholder = "Enter NABat password here"),
  textInput("enter_project", "NABat Project Number(s)", value = "",
            placeholder = "Enter relevant NABat project number(s), separated by a comma",
            width = '50%'),
  textInput("enter_grts", "Enter GRTS Cell Number(s)", value = "", 
            placeholder = "Enter GRTS cell numbers separated by a comma.",
            width = '50%'),
  textInput("enter_locations", "Survey Location Name(s)", 
            value = "", placeholder = "Enter survey location names separated by a comma. Leave blank if you want all sites within the cell included.",
            width = '50%'),
  selectInput("hub", "Which Hub is this report for?", choices = c("PacWest", "Southwest"),
              selected = NULL),
  downloadButton("report", "Generate Report")
)

# Define server
server <- function(input, output, session) {
  
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      withProgress(message = 'Rendering, please wait!', {
        
        tempReport <- file.path(here::here("src", paste(input$hub, "landowner", "reporting.Rmd", sep = "_")))
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(project = input$enter_project, username = input$enter_username, 
                       password = input$enter_password, grts = input$enter_grts, 
                       locations = input$enter_locations)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))

      })})
}

shinyApp(ui = ui, server = server)
