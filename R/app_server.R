#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  res_auth <- shinymanager::secure_server(check_credentials = shinymanager::check_credentials(credentials))

  
  onStop(function() cat("Session stopped\n"))
  
  
  # disconnected <- sever::sever_default(
  #   title = "Disconnected",
  #   subtitle = "Your session ended",
  #   button = "Reconnect"
  # )
  
  # sever::sever(html = disconnected, bg_color = "#005c9c")

  backgroundchange <- reactive({
      invalidateLater(1000, session)

      runif(1)
    })
   conn <- golem::get_golem_options("conn_SQL_Lite")
  
  # Reactive value for table names
  table_names <- reactiveVal(DBI::dbListTables(conn))

  loaded_view_table <- FALSE
  loaded_update_table <- FALSE
  loaded_about <- FALSE

  observeEvent(input$sidebarmenu, {
    #LAZY LOADING

    if(input$sidebarmenu == "view_table" & !loaded_view_table){
      loaded_view_table <<- TRUE
      mod_view_table_server("view_table_1", table_names)
    }

    if(input$sidebarmenu == "update_table" & !loaded_update_table){
      loaded_update_table <<- TRUE
      mod_update_table_server("update_table_1", table_names)
    }

    if(input$sidebarmenu == "about" & !loaded_about){
      loaded_about <<- TRUE
      # mod_about_server("about_1")
    }
  })
  # ...
}
