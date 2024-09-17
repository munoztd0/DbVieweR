#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp modalDialog showModal reactiveVal
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  # Check if it's first time setup
  credentials <- retrieve_credentials("app_config.sqlite")
  
  if (is.null(credentials)) {
    # Run first time setup
    setup_result <- first_time_setup()
    if (is.null(setup_result)) {
      return(invisible())  # User cancelled setup
    }
    credentials <- setup_result
  }
  
  # Create a reactive value to store the connection
  conn <- reactiveVal(NULL)
  
  # Modify app_ui to include password input if needed
  modified_app_ui <- function(request) {
    tagList(
      if (credentials$type != "SQLite") {
        modalDialog(
          title = "Database Password",
          passwordInput("db_password", "Enter your database password:"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "OK")
          )
        )
      },
      app_ui(request)
    )
  }
  
  # Modify app_server to handle password input and connection
  modified_app_server <- function(input, output, session) {
  if (credentials$type != "SQLite") {
    observeEvent(input$ok, {
      password <- input$db_password
      
      tryCatch({
        db_conn <- create_conn_from_details(credentials, password)
        conn(db_conn)
        removeModal()  # Remove the modal after successful connection
        showNotification("Connected to database successfully", type = "message")
      }, error = function(e) {
        showNotification(paste("Connection failed:", e$message), type = "error")
      })
    })
  } else {
    # For SQLite demo, create connection without password
    db_conn <- create_conn_from_details(credentials)
    conn(db_conn)
  }
  
  # Wait for connection to be established before running app_server
  observe({
    if (!is.null(conn())) {
      app_server(input, output, session)
    }
  })
}

  
  
  with_golem_options(
    app = shinyApp(
      ui = modified_app_ui,
      server = modified_app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
      "credentials" = credentials,
      "conn" = function() conn()
    )
  )
}