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
  selectInput("hub", "Whho is this report for?", choices = c("ndpl"),
              selected = NULL),
  downloadButton("report", "Generate Report")
)

server <- function(input, output, session) {
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      withProgress(message = 'Rendering, please wait!', {
        tempReport <- file.path(here::here("src", paste(input$hub, "reporting.Rmd", sep = "_")))
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

    params <- list(project = input$enter_project, username = input$enter_username,
                 password = input$enter_password, grts = input$enter_grts,
                 locations = input$enter_locations)

    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv()))
          }
        )
      }
    )
}

shinyApp(ui = ui, server = server)