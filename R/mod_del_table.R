#' del_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_del_table_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' del_table Server Functions
#'
#' @noRd 
mod_del_table_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_del_table_ui("del_table_1")
    
## To be copied in the server
# mod_del_table_server("del_table_1")
