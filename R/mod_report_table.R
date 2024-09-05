#' Create Report UI Function
#'
#' @description A shiny Module for creating reports using DataExplorer.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_create_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Create Report", 
        status = "primary", 
        solidHeader = TRUE, 
        width = 12,
        actionButton(ns("generate_report"), "Generate Report", class = "btn-primary"),
        br(),
        br(),
        uiOutput(ns("report_iframe"))
      )
    )
  )
}#' Create Report UI Function
#'
#' @description A shiny Module for creating reports using DataExplorer.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_create_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Create Report", 
        status = "primary", 
        solidHeader = TRUE, 
        width = 12,
        actionButton(ns("generate_report"), "Generate Report", class = "btn-primary"),
        br(),
        br(),
        uiOutput(ns("report_iframe"))
      )
    )
  )
}


#' Create Report Server Function
#'
#' @noRd
mod_create_report_server <- function(id, selected_table_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Observe when the report is generated
    observeEvent(input$generate_report, {
      req(selected_table_data())  # Make sure data is available
      
      # Limit the dataset to 10,000 rows
      data <- head(selected_table_data(), 10000)
      
      # Create a temporary file to store the report
      report_file <- tempfile(fileext = ".html")
      
      # Generate the report using DataExplorer
      DataExplorer::create_report(data, output_file = report_file)
      
      # Render the iframe to display the report on the same page
      output$report_iframe <- renderUI({
        tags$iframe(
          src = report_file,
          width = "100%",
          height = "800px",
          frameborder = 0
        )
      })
    })
  })
}

