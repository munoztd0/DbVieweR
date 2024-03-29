#' update_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 

mod_update_table_ui <- function(id){

  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow( 
        box(width = 12,
            title = "Note:",
            "You can only alter tables that are created in the ",
            tags$i("Create Tables "),
            "tab. If you find the dropdown menu empty, this means currently no table can be altered.",
            style = "font-size: 15px;"),
        box(title = 'Rename Table', width = 4, solidHeader = TRUE, status = "primary",
            uiOutput(ns('sel_table_1_ui')),  # Use uiOutput here
            wellPanel(
              textInput(inputId = ns("rnm_table_to"),
                        label = "Rename To:"),
              actionButton(inputId = ns("rename_table"),
                           label = "Rename Table", 
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4; display: right-align" )
            )
        )#,
        # box(title = 'Rename Column', width = 4, solidHeader = TRUE, status = "primary",
        #     uiOutput(ns('sel_table_2_ui')),  # Use uiOutput here
        #     wellPanel(
        #       selectInput(
        #         inputId = ns('sel_col_3'),
        #         label = 'Select Column:',
        #         choices = NULL),
        #       textInput(inputId = ns("rnm_col_to"),
        #                 label = "Rename the Column:"),
        #       actionButton(inputId = ns("rename_col"),
        #                    label = "Rename Column", 
        #                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
        #       )
        #     )
        # ),
        # box(title = 'Add Column', width = 4, solidHeader = TRUE, status = "primary",
        #    uiOutput(ns('sel_table_3_ui')),  # Use uiOutput here
        #    wellPanel(
        #      textInput(inputId = ns('add_col_name'), label = "Add Column"),
        #      selectInput(inputId = ns("add_col_type"), label = "Add Column Type", 
        #                  choices = c("NUMERIC","VARCHAR(255)","BOOLEAN")
        #      ),
        #      actionButton(inputId = ns("add_col"),
        #                   label = "Add Column",
        #                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        #    )
        # )
      )
    )
  )
}


#' update_table Server Functions
mod_update_table_server <- function(id, table_names){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    conn <- golem::get_golem_options("conn_SQL_Lite")

    res_auth <- shinymanager::secure_server(check_credentials = shinymanager::check_credentials(credentials))

    # Create reactive values including all credentials
    creds_reactive <- reactive({
      reactiveValuesToList(res_auth)
    })

    # Render UI for table selection in 'Rename Table' box
    output$sel_table_1_ui <- renderUI({
      selectInput(ns("sel_table_1"), "Select table", choices = table_names())
    })

    observeEvent(input$rename_table, {
      req(input$rnm_table_to)
      DBI::dbExecute(conn, paste0('ALTER TABLE ', input$sel_table_1, ' RENAME TO ', input$rnm_table_to))
      # Update table_names reactive value
      table_names(DBI::dbListTables(conn))
      showModal(modalDialog(
        title = "Success",
        "Table renamed successfully!",
        easyClose = TRUE
      ))
    })

    # Rest of the code...
  })
}

## To be copied in the UI
# mod_update_table_ui("update_table_1")
    
## To be copied in the server
# mod_update_table_server("update_table_1")
