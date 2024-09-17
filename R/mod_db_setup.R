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
modified_app_server <- function(input, output, session) {
  if (credentials$type != "SQLite") {
    observeEvent(input$ok, {
      password <- input$db_password
      
      tryCatch({
        db_conn <- create_conn_from_details(credentials, password)
        conn(db_conn)
        
        # Ensure modal is removed after successful connection
        shiny::removeModal()
        shiny::showNotification("Connected to database successfully", type = "message")
        
      }, error = function(e) {
        shiny::showNotification(paste("Connection failed:", e$message), type = "error")
      })
    })
  } else {
    # For SQLite demo, create connection without password
    db_conn <- create_conn_from_details(credentials)
    conn(db_conn)
  }
  
  # Observe when connection is established and then run app_server
  observe({
    if (!is.null(conn())) {
      req(conn())  # Ensure connection is not NULL
      app_server(input, output, session)
    }
  })
}
