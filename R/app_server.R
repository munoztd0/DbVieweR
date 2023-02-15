#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  result_auth <- shinymanager::secure_server(check_credentials = shinymanager::check_credentials(credentials))
  
  # output$res_auth <- renderPrint({
  #   reactiveValuesToList(result_auth)
  # })
  
  
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
  
  # call login module supplying data frame, user and password cols
  # and reactive trigger

  
  # credentials <- shinyauthr::loginServer("login", 
  #                           data = user_base,
  #                           user_col = user,
  #                           pwd_col = password_hash,
  #                           sessionid_col = sessionid,
  #                           sodium_hashed = TRUE,  
  #                           cookie_getter = get_sessionids_from_db,
  #                           cookie_setter = add_sessionid_to_db,
  #                           log_out = reactive(logout_init()))
  # 
  # logout_init <- shinyauthr::logoutServer(
  #   id = "logout",
  #   active = reactive(credentials()$user_auth)
  # )
  
  
  
  # Add or remove a CSS class from an HTML element
  # Here sidebar-collapse
  # observe({
  #   if(credentials()$user_auth) {
  #     shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  #   } else {
  #     shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  #   }
  # })
  # 
  

  
  
  
  # Show the sample login info
  # output$user_table <- renderUI({
  #   # only show pre-login
  #   if(credentials()$user_auth) return(NULL)
  #   fluidRow(column(6,
  #                   p("Please use the usernames and passwords provided below to test this database management platform. 
  #                     There are two kinds of accounts: admin and manager. 
  #                     The manager account allows one to alter tables, columns, and records, and delete tables. 
  #                     The admin account allows one to do everything above except for deleting tables.", 
  #                     class = "text-center", style = "font-size: 15px;"),
  #                   br(),
  #                   renderTable({user_base[, -3]}), offset = 3
  #   )
  #   )
  # })
  
  # pulls out the user information returned from login module
  # user_info <- reactive({credentials()$info})
  # 
  # # menu welcome info
  # output$welcome <- renderText({
  #   req(credentials()$user_auth)
  #   
  #   
  #   #paste0("Welcome ",{user_info()$permissions},"!")
  #   
  # })
  
  

    loaded_view_table   <- TRUE
    # test = "now way"

    
    observeEvent(input$sidebarmenu, {


      #TRADING

      if(input$sidebarmenu == "view_table" & !loaded_view_table){
        
         # req(credentials()$user_auth)
         # Listener1 <- input$refresh
         # isolate(backgroundchange())
        
        browser()
         loaded_view_table <<- TRUE
         mod_view_table_server("view_table_1")
         
       }
    })

}
