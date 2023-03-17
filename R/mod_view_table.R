#' view_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom DBI dbListTables
#' @importFrom DT DTOutput
#' 

mod_view_table_ui <- function(id){
  
  
  conn <- golem::get_golem_options("conn_SQL_Lite")
  
  
  ns <- NS(id)
  
  

  fluidPage(
    fluidRow(
      column(1),
      column(10,
             fluidRow(
               column(12,
                      
                      selectInput(
                        inputId = ns('sel_table_1'),
                        label = 'Tables in Database',
                        choices = sort(DBI::dbListTables(conn), decreasing = T)
                          
                      ),
                      
                      h4(strong("Table Preview")),
                      br(),
                      DT::DTOutput(
                        outputId = ns('sel_table_view'))
                      )
                      
               )
               
               
               
      )
             
             
    )
      
      
   )
  
  
}

#' view_table Server Functions
#'
#'
#' @import dplyr
#' @importFrom DT renderDT datatable
#'
#' @noRd 
mod_view_table_server <- function(id){
  moduleServer( id, function(input, output, session){
    
    
    ns <- session$ns
    
    conn <- golem::get_golem_options("conn_SQL_Lite")
    
    # show table
    output$sel_table_view <- DT::renderDT({
      
      
      dplyr::tbl(conn, input$sel_table_1)  |> collect() |> 
        
      DT::datatable(
        filter = "top",
        extensions = 'Scroller',
        options = list(deferRender = F, dom = 'Bfrtip',
                       columnDefs = list(list(className = 'dt-left',
                                              targets = "_all")),
                       scroller = TRUE, scrollX = T, scrollY = 500,
                       pageLength = 20, searching = TRUE))


    })
    
  
      
      
      

  })
}

## To be copied in the UI
# mod_view_table_ui("view_table_1")

## To be copied in the server
# mod_view_table_server("view_table_1")
