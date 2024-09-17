#' helpers 
#'
#' @description A connection function
#' 
#' 
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect dbWriteTable
#'
#' @return connection#' helpers 
#'
#' @description A connection function
#' 
#' 
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect dbWriteTable
#'
#' @return connection
#'
#' @noRd


create_conn <- function(){
  

  if(file.exists("test_db_file")) {
    # unlink("test_db_file")
    db <- dbConnect(SQLite(), 'test_db_file')
    print("DB already exists")
    
    print("Database already existsed")
  } else {
    db <- dbConnect(SQLite(), 'test_db_file')
    print("DB doesn't exist")

  }


  dbWriteTable(db, "iris", iris, overwrite =T)
  dbWriteTable(db, "mtcars", mtcars, overwrite =T)
  dbWriteTable(db, "starwars2", starwars2, overwrite =T)
  
  return(db)
}


# TODO: Remove this mess before merging to main



#' Store Credentials Securely in SQLite
#'
#' @description Hashes passwords and stores user credentials in an SQLite database.
#'
#' @param details A list containing database connection details, including `type`, `host`, `port`, `name`, `user`, and `password`.
#' @param db_file Path to the SQLite database file.
#'
#' @return NULL invisibly. Throws an error if storage fails.
#'
#' @examples
#' \dontrun{
#' details <- list(
#'   type = "PostgreSQL",
#'   host = "localhost",
#'   port = "5432",
#'   name = "mydb",
#'   user = "user",
#'   password = "password"
#' )
#' store_credentials(details, "app_config.sqlite")
#' }
store_credentials <- function(details, db_file) {
  # if (!all(c("type", "host", "port", "name", "user", "password") %in% names(details))) {
  #   stop("Details must contain 'type', 'host', 'port', 'name', 'user', and 'password' elements.")
  # }
  
  # Hash the password
  password_hash <- sodium::password_store(details$password)
  
  # Connect to SQLite database
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  # Create table if it doesn't exist
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS credentials (
      id INTEGER PRIMARY KEY,
      type TEXT,
      host TEXT,
      port TEXT,
      name TEXT,
      user TEXT,
      password_hash TEXT
    )
  ")
  
  # Insert the credentials into the database
  DBI::dbExecute(conn, "
    INSERT INTO credentials (type, host, port, name, user, password_hash)
    VALUES (?, ?, ?, ?, ?, ?)
  ", list(details$type, details$host, details$port, details$name, details$user, details$password))
}




#' Verify User Credentials from SQLite
#'
#' @description Retrieves and verifies the provided password against the stored hashed password in SQLite.
#'
#' @param user The username to verify.
#' @param password The password to verify.
#' @param db_file Path to the SQLite database file.
#'
#' @return A list with user details if the password is correct. Throws an error if verification fails.
#'
#' @examples
#' \dontrun{
#' user_info <- verify_credentials("user", "password", "app_config.sqlite")
#' }
retrieve_credentials <- function(db_file) {
  # Connect to SQLite database
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  # Retrieve the credentials for the specified user
  result <- DBI::dbGetQuery(conn, "
    SELECT type, host, port, name, user, password_hash 
    FROM credentials 
  ")
  
  
  # Verify the password
  # is_valid <- sodium::password_verify(result$password_hash[1], password)
  # print("here")
  # browser()
  # if (is_valid) {
  return(as.list(result[1, ]))
  # } else {
  #   stop("Invalid password.")
  # }
}



#' Install Database Backend Package
#'
#' @description Installs the appropriate DBI backend package based on the selected database type.
#'
#' @param db_type A string specifying the database type.
#'
#' @return TRUE if installation was successful, FALSE otherwise.
#'
#' @importFrom utils install.packages
install_db_backend <- function(db_type) {
  required_package <- switch(db_type,
    "PostgreSQL" = "RPostgres",
    "MySQL" = "RMariaDB",
    "SQLite" = "RSQLite",
    "SQL Server" = "odbc",
    "BigQuery" = "bigrquery",
     showModal(modalDialog(
            title = "Failure",
            "Unsupported database type",
            easyClose = TRUE
        ))
  )
  command <- switch(required_package,
          "RPostgres" = "RPostgres::Postgres()",
          "RMariaDB" = "RMariaDB::MariaDB()",
          "SQLite" = "SQLite::SQLite()",
          "odbc" = "odbc::odbc()",
          "bigrquery" = "bigrquery::bigquery()"
      )

  if (!requireNamespace(required_package, quietly = TRUE)) {
    tryCatch({
      install.packages(required_package, repos = "https://cloud.r-project.org")
      library(required_package, character.only = TRUE)
      return(command)
    }, error = function(e) {
      message("Failed to install ", required_package, ": ", e$message)
 
      return(FALSE)
    })
  } else {
    message(required_package, " is already installed.")
    return(command)
  }
}

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
    h2("Database Setup"),
    radioButtons(ns("db_choice"), "Choose your database setup:",
                 choices = c("Use dummy SQLite setup (demo)" = "sqlite",
                             "Connect to your own database" = "custom")),
    conditionalPanel(
      condition = "input.db_choice == 'sqlite'", ns = ns,
      actionButton(ns("use_sqlite"), "Continue with SQLite Demo")
    ),
    conditionalPanel(
      condition = "input.db_choice == 'custom'", ns = ns,
      selectInput(ns("db_type"), "Database type:", 
                  choices = c("PostgreSQL", "MySQL", "SQLite", "SQL Server")),
      textInput(ns("db_host"), "Host:"),
      textInput(ns("db_port"), "Port:"),
      textInput(ns("db_name"), "Database name:"),
      textInput(ns("db_user"), "Username:"),
      passwordInput(ns("db_password"), "Password:"),
      checkboxInput(ns("store_credentials"), "Store connection details for next time?"),
      actionButton(ns("test_connection"), "Test Connection"),
      actionButton(ns("save_connection"), "Save and Continue")
    ),
    textOutput(ns("connection_status"))
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
      details <- list(type = "sqlite", name = "test_db_file")
      connection_details(details)
      connection_status("Using SQLite demo setup. Click 'Save and Continue' to proceed.")
    })
    
    # Test custom database connection
    observeEvent(input$test_connection, {
      req(input$db_choice == "custom")
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
        connection_status("Connection successful! Click 'Save and Continue' to proceed.")
      }, error = function(e) {
        connection_status(paste("Connection failed:", e$message))
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
          user = input$db_user,
          password = input$db_password
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

#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(onStart = NULL,
                    options = list(),
                    enableBookmarking = NULL,
                    uiPattern = "/",
                    ...) {
  
  # Check if it's first time setup
  if (!file.exists("app_config.sqlite")) {
    # Run first time setup
    setup_result <- first_time_setup()
    if (is.null(setup_result)) {
      return(invisible())  # User cancelled setup
    }
  }
  
  # Load connection details
  conn_details <- tryCatch({
    print("there")
    retrieve_credentials("app_config.sqlite")
  }, error = function(e) {
    print("here failed")
    showMessage("Failed to retrieve connection details. Using SQLite demo setup.")
    #list(type = "sqlite", name = "test_db_file")  # Default to SQLite if retrieval fails
  })
  
  # Create database connection
  conn <- create_conn_from_details(conn_details)


  # when exiting app, disconnect from database
  onStop(function() {
    cat("Doing application cleanup\n")
    DBI::dbDisconnect(conn)
    # if (conn_details$type == "sqlite" && conn_details$name == "test_db_file") {
    #   unlink("test_db_file")
    # }
  })
  
  with_golem_options(
    app = shinyApp(
      ui = shinymanager::secure_app(app_ui, head_auth = tags$script(inactivity), theme = shinythemes::shinytheme("flatly")),
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list("conn" = conn)


  )
}

#' First Time Setup
#'
#' @description Runs the first-time setup process for database configuration.
#'
#' @return A list of connection details if setup is completed, NULL if cancelled.
first_time_setup <- function() {
  ui <- fluidPage(
    mod_db_setup_ui("db_setup")
  )
  
  server <- function(input, output, session) {
    setup_result <- mod_db_setup_server("db_setup")
    
    observeEvent(setup_result$connection_details(), {
      # Setup completed, return the connection details
      stopApp(setup_result$connection_details())
    })
  }
  
  shiny::runApp(shinyApp(ui, server))
}

#' Create Database Connection from Details
#'
#' @param details A list containing database connection details.
#'
#' @return A database connection object.
create_conn_from_details <- function(details) {
 tryCatch({
    return(
     DBI::dbConnect(
          eval(parse(text = install_db_backend(details$type))),
          host = details$host,
          port = as.integer(details$port),
          dbname = details$name,
          user = details$user,
          password = details$password_hash
        )

    )
  }, error = function(e) {
    # Add logic for other database types here
    stop("Database type not supported.")
  })
}

# Set up connection cleanup
my_cleanup <- function() {
    if (!is.null(conn()) && credentials$type != "SQLite") {
      DBI::dbDisconnect(conn())
    }
    if (credentials$type == "SQLite" && credentials$name == "test_db_file") {
      unlink("test_db_file")
    }
  }

# 
create_conn <- function(){
  

  if(file.exists("test_db_file")) {
    # unlink("test_db_file")
    db <- dbConnect(SQLite(), 'test_db_file')
    print("DB already exists")
    
    print("Database already existsed")
  } else {
    db <- dbConnect(SQLite(), 'test_db_file')
    print("DB doesn't exist")

  }


  dbWriteTable(db, "iris", iris, overwrite =T)
  dbWriteTable(db, "mtcars", mtcars, overwrite =T)
  dbWriteTable(db, "starwars2", starwars2, overwrite =T)
  
  return(db)
}


# TODO: Remove this mess before merging to main



#' Store Credentials
#'
#' @description Stores user credentials in an SQLite database.
#'
#' @param details A list containing database connection details.
#' @param db_file Path to the SQLite database file.
#'
#' @return NULL invisibly. Throws an error if storage fails.
#'
#' @noRd
store_credentials <- function(details, db_file) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS credentials (
      id INTEGER PRIMARY KEY,
      type TEXT,
      host TEXT,
      port TEXT,
      name TEXT,
      user TEXT
    )
  ")
  
  DBI::dbExecute(conn, "
    INSERT INTO credentials (type, host, port, name, user)
    VALUES (?, ?, ?, ?, ?)
  ", list(details$type, details$host, details$port, details$name, details$user))
}


#' Retrieve Credentials
#'
#' @description Retrieves stored credentials from SQLite.
#'
#' @param db_file Path to the SQLite database file.
#'
#' @return A list with user details if found, NULL otherwise.
#'
#' @noRd
retrieve_credentials <- function(db_file) {
  if (!file.exists(db_file)) {
    return(NULL)
  }
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  
  result <- DBI::dbGetQuery(conn, "SELECT * FROM credentials LIMIT 1")
  
  if (nrow(result) == 0) {
    return(NULL)
  }
  
  as.list(result)
}


#' First Time Setup
#'
#' @description Runs the first-time setup process for database configuration.
#'
#' @return A list of connection details if setup is completed, NULL if cancelled.
first_time_setup <- function() {
  ui <- fluidPage(
    mod_db_setup_ui("db_setup")
  )
  
  server <- function(input, output, session) {
    setup_result <- mod_db_setup_server("db_setup")
    
    observeEvent(setup_result$connection_details(), {
      # Setup completed, return the connection details
      stopApp(setup_result$connection_details())
    })
  }
  
  shiny::runApp(shinyApp(ui, server))
}



#' Create Database Connection from Details
#'
#' @param details A list containing database connection details.
#'
#' @return A database connection object.
create_conn_from_details <- function(details, password = NULL) {
  if (details$type == "SQLite" && details$name == "test_db_file") {
    return(create_conn())  # Use the existing function for SQLite demo
  }
  
  # For custom database connections
  tryCatch({
    DBI::dbConnect(
      eval(parse(text = install_db_backend(details$type))),
      host = details$host,
      port = as.integer(details$port),
      dbname = details$name,
      user = details$user,
      password = password
    )
  }, error = function(e) {
    stop(paste("Failed to connect to database:", e$message))
  })
}



# Keep the existing install_db_backend function
install_db_backend <- function(db_type) {
  required_package <- switch(db_type,
    "PostgreSQL" = "RPostgres",
    "MySQL" = "RMariaDB",
    "SQLite" = "RSQLite",
    "SQL Server" = "odbc",
    "BigQuery" = "bigrquery",
    stop("Unsupported database type")
  )
  command <- switch(required_package,
    "RPostgres" = "RPostgres::Postgres()",
    "RMariaDB" = "RMariaDB::MariaDB()",
    "RSQLite" = "RSQLite::SQLite()",
    "odbc" = "odbc::odbc()",
    "bigrquery" = "bigrquery::bigquery()"
  )

  if (!requireNamespace(required_package, quietly = TRUE)) {
    tryCatch({
      install.packages(required_package, repos = "https://cloud.r-project.org")
      library(required_package, character.only = TRUE)
      return(command)
    }, error = function(e) {
      message("Failed to install ", required_package, ": ", e$message)
      return(FALSE)
    })
  } else {
    message(required_package, " is already installed.")
    return(command)
  }
}