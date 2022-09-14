library(shiny)
library(tidyverse)
library(here)

lookup_t <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1UggTTGxYYmVM2FVpjHqn-q0AZLb-oqGKlJytg7r5JXg/edit?usp=sharing')
forest_ids <- unique(lookup_t$ForestID)

ui <- fluidPage(
  tags$head(tags$style(".shiny-notification {position: fixed; top: 25%; left: 25%; font-size: 30px}")),
  titlePanel("NDPL Reporting Tool"),
  textInput("enter_username", "NABat Username", value = "",
            placeholder = "Enter NABat username here"),
  textInput("enter_password", "NABat Password", value = "",
            placeholder = "Enter NABat password here"),
  textInput("enter_project", "NABat Project Number(s)", value = "",
            placeholder = "Enter relevant NABat project number(s), separated by a comma",
            width = '50%'),
  textInput("enter_grts", "Enter GRTS Cell Number(s)", value = "", 
            placeholder = "Enter GRTS cell numbers separated by a comma. Leave blank if you want all GRTS cells within a project",
            width = '50%'),
  textInput("enter_locations", "Survey Location Name(s)", 
            value = "", placeholder = "Enter survey location names separated by a comma. Leave blank if you want all sites within the cell included.",
            width = '50%'),
  textInput("partner", "Who is this report for?", value = "", 
            placeholder = "Enter partner name as you want it to appear in the report",
            width = '50%'),
  selectInput("forest", "What is the Forest ID in the Google Drive?", choices = forest_ids,
              selected = NULL),
  textInput("freq_table", "Enter file path to frequency table", value = ""),
  downloadButton("report", "Generate Report")
)

server <- function(input, output, session) {
  
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      withProgress(message = 'Rendering, please wait!', {
        tempReport <- file.path(here::here("src", "ndpl_reporting.rmd"))
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

    params <- list(project = input$enter_project, username = input$enter_username,
                 password = input$enter_password, grts = input$enter_grts,
                 locations = input$enter_locations, 
                 agency = input$partner,
                 forest_id = input$forest,
                 table_path = input$freq_table)

    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv()))
          }
        )
      }
    )
}

shinyApp(ui = ui, server = server)