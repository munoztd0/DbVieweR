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
#' 

mod_view_table_ui <- function(id){
  
  
  conn <- golem::get_golem_options("conn_SQL_Lite")
  
  
  ns <- NS(id)
  
  
  box(width = NULL, status = "primary",
      wellPanel(
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = ns('sel_table_1'),
              label = 'Tables in Database',
              choices = DBI::dbListTables(conn)
            )
          ),
          mainPanel(
            h4(strong("Table Preview")),
            br(),
            dataTableOutput(
              outputId = ns('sel_table_view'))
          )
        )
      )
  )
  
  
}

#' view_table Server Functions
#'
#' @noRd 
mod_view_table_server <- function(id){
  moduleServer( id, function(input, output, session){
    
    
    ns <- session$ns
    
    conn <- golem::get_golem_options("conn_SQL_Lite")
    
    # show table
    output$sel_table_view <- DT::renderDT({
      
      tbl(conn, input$sel_table_1) %>% DT::datatable(
        filter = "top",
        extensions = 'Scroller',   
        options = list(deferRender = F, dom = 'Bfrtip',
                       columnDefs = list(list(className = 'dt-left',
                                              targets = "_all")),
                       scroller = TRUE, scrollX = T, #scrollY = 600,
                       pageLength = 20, searching = TRUE))
      
      
    })
    
  
      
      
      

  })
}

## To be copied in the UI
# mod_view_table_ui("view_table_1")

## To be copied in the server
# mod_view_table_server("view_table_1")
