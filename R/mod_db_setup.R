# mod_db_setup.R

#' DB Setup UI Function
#'
#' @description A shiny Module for first-time database setup.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_db_setup_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6, offset = 3,
        wellPanel(
          h2("Database Setup", style = "text-align: center;"),
          radioButtons(ns("db_choice"), "Choose your database setup:",
                       choices = c("Use dummy SQLite setup (demo)" = "sqlite",
                                   "Connect to your own database" = "custom")),
          conditionalPanel(
            condition = "input.db_choice == 'sqlite'", ns = ns,
            actionButton(ns("use_sqlite"), "Continue with SQLite Demo", 
                         class = "btn-primary btn-block")
          ),
          conditionalPanel(
            condition = "input.db_choice == 'custom'", ns = ns,
            selectInput(ns("db_type"), "Database type:", 
                        choices = c("PostgreSQL", "MySQL", "SQLite", "SQL Server", "BigQuery")),
            textInput(ns("db_host"), "Host:"),
            textInput(ns("db_port"), "Port:"),
            textInput(ns("db_name"), "Database name:"),
            textInput(ns("db_user"), "Username:"),
            passwordInput(ns("db_password"), "Password:"),
            checkboxInput(ns("store_credentials"), "Store connection details for next time?"),
            actionButton(ns("test_connection"), "Test Connection", 
                         class = "btn-info btn-block"),
            actionButton(ns("save_connection"), "Save and Continue", 
                         class = "btn-success btn-block")
          ),
          textOutput(ns("connection_status"))
        )
      )
    )
  )
}

#' DB Setup Server Function
#'
#' @noRd 
mod_db_setup_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Reactive values to store connection details and status
    connection_details <- reactiveVal(NULL)
    connection_status <- reactiveVal(NULL)
    
    # Handle SQLite demo setup
    observeEvent(input$use_sqlite, {
      details <- list(type = "SQLite", name = "test_db_file")
      connection_details(details)
      connection_status("Using SQLite demo setup. Click 'Save and Continue' to proceed.")
    })
    
    # Test custom database connection
    observeEvent(input$test_connection, {
      req(input$db_choice == "custom")
      browser()
      tryCatch({
        conn <- DBI::dbConnect(
          eval(parse(text = install_db_backend(input$db_type))),
          host = input$db_host,
          port = as.integer(input$db_port),
          dbname = input$db_name,
          user = input$db_user,
          password = input$db_password
        )
        DBI::dbDisconnect(conn)
        showModal(modalDialog(
            title = "Connection Status",
            "Connection successful! Click 'Save and Continue' to proceed.",
            easyClose = TRUE
            ))
      }, error = function(e) {
        showModal(modalDialog(
            title = "Connection Error",
            paste("Connection failed:", e$message),
            easyClose = TRUE
            ))
      })
    })
    
    # Save connection details
    observeEvent(input$save_connection, {
      if(input$db_choice == "custom") {
        details <- list(
          type = input$db_type,
          host = input$db_host,
          port = input$db_port,
          name = input$db_name,
          user = input$db_user
        )
        connection_details(details)
        
        if(input$store_credentials) {
          tryCatch({
            store_credentials(details, "app_config.sqlite")
            connection_status("Credentials stored successfully! Setup complete.")
          }, error = function(e) {
            connection_status(paste("Failed to store credentials:", e$message))
          })
        } else {
          connection_status("Setup complete. Credentials not stored.")
        }
      } else {
        # Confirm SQLite setup
        connection_status("SQLite demo setup confirmed. Setup complete.")
      }
    })
    
    # Display connection status
    output$connection_status <- renderText({
      connection_status()
    })
    
    # Return reactive values to be used in main app
    return(list(
      connection_details = connection_details,
      connection_status = connection_status
    ))
  })
}