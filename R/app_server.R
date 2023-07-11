#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  


  res_auth <- shinymanager::secure_server(check_credentials = shinymanager::check_credentials(credentials))


  shinymetrics::shinymetrics_server(token = "CQDPXTAORH7X7AP5DTXMLTL3GI") # (optional) print information on tracking
    
  

  
  onStop(function() cat("Session stopped\n"))
  
  
  disconnected <- sever::sever_default(
    title = "Disconnected",
    subtitle = "Your session ended",
    button = "Reconnect"
  )
  
  sever::sever(html = disconnected, bg_color = "#005c9c")

  backgroundchange <- reactive({
      invalidateLater(1000, session)

      runif(1)
    })
  
  
    mod_view_table_server("view_table_1")
    loaded_about   <- FALSE

    
    observeEvent(input$sidebarmenu, {


      #LOADING

      if(input$sidebarmenu == "about" & !loaded_about){
        
        
        loaded_about <<- TRUE
        mod_about_server("about_1")
        
      }
      
      
      
      
    })

}
