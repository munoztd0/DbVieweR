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
                      
               ),
              fluidRow(  column(12, 
                          downloadButton(ns('download'),"Download the data")          
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
#' @importFrom openxlsx createWorkbook saveWorkbook
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

      res_auth <- shinymanager::secure_server(check_credentials = shinymanager::check_credentials(credentials))


  # Create reactive values including all credentials
  creds_reactive <- reactive({
    reactiveValuesToList(res_auth)
  })


     # Hide extraOutput only when condition is TRUE
  observe({
    if (!is.null(creds_reactive()$level) && creds_reactive()$level > 0 ) {
      
      print("show")

      output$download <- 
        downloadHandler(
          filename = paste0("data_", input$sel_table_1, ".xlsx"),
          content = function(file){
            db <- dplyr::tbl(conn, input$sel_table_1)  |> collect()
            wb <- openxlsx::createWorkbook()
            style_my_workbook(wb, db[input[["sel_table_view_rows_all"]], ], "Sheet1", TRUE)
            openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
          }
        )


    } 
  })
    
  
      
      
      

  })
}

## To be copied in the UI
# mod_view_table_ui("view_table_1")

## To be copied in the server
# mod_view_table_server("view_table_1")
