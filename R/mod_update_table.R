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
            tags$i("Create Tables "),
            "tab. If you find the dropdown menu empty, this means currently no table can be altered.",
            style = "font-size: 15px;"),
        box(title = 'Rename Table', width = 4, solidHeader = TRUE, status = "primary",
            uiOutput(ns('sel_table_1_ui')),
            wellPanel(
              textInput(inputId = ns("rnm_table_to"),
                        label = "Rename To:"),
              actionButton(inputId = ns("rename_table"),
                           label = "Rename Table", 
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4; display: right-align" )
            )
        ),
        box(title = 'Rename Column', width = 4, solidHeader = TRUE, status = "warning",
            uiOutput(ns('sel_table_2_ui')),
            uiOutput(ns('sel_column_ui')),
            wellPanel(
              textInput(inputId = ns("rnm_column_to"),
                        label = "Rename To:"),
              actionButton(inputId = ns("rename_column"),
                           label = "Rename Column", 
                           style="color: #fff; background-color: #f0ad4e; border-color: #eea236; display: right-align" )
            )
        ),
        box(title = 'Delete Table', width = 4, solidHeader = TRUE, status = "danger",
            uiOutput(ns('sel_table_4_ui')),
            wellPanel(
              actionButton(inputId = ns("delete_table"),
                           label = "Delete Table",
                           style="color: #fff; background-color: #d9534f; border-color: #d43f3a; display: right-align")
            )
        )
      )
    )
  )
}


#' update_table Server Functions
#'
#' @description A table can be renamed or deleted, and columns can be renamed.
#'
#' @param id 
#' @import DBI
#' @importFrom shiny showModal modalDialog
#'
#' @noRd
mod_update_table_server <- function(id, table_names){
  moduleServer(id, function(input, output, session){
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

    # Render UI for table selection in 'Rename Column' box
    output$sel_table_2_ui <- renderUI({
      selectInput(ns("sel_table_2"), "Select table", choices = table_names())
    })

    # Render UI for column selection in 'Rename Column' box
    output$sel_column_ui <- renderUI({
      req(input$sel_table_2)
      column_names <- DBI::dbListFields(conn, input$sel_table_2)
      selectInput(ns("sel_column"), "Select column", choices = column_names)
    })

    # Render UI for table selection in 'Delete Table' box
    output$sel_table_4_ui <- renderUI({
      selectInput(ns("sel_table_4"), "Select table", choices = table_names())
    })

    observeEvent(input$rename_table, {
      req(input$rnm_table_to)
      DBI::dbExecute(conn, paste0('ALTER TABLE "', input$sel_table_1, '" RENAME TO "', input$rnm_table_to, '"'))
      # Update table_names reactive value
      table_names(DBI::dbListTables(conn))
      showModal(modalDialog(
        title = "Success",
        "Table renamed successfully!",
        easyClose = TRUE
      ))
    })

    observeEvent(input$rename_column, {
      req(input$rnm_column_to)
      tryCatch({
        DBI::dbExecute(conn, sprintf('ALTER TABLE "%s" RENAME COLUMN "%s" TO "%s"', 
                                     input$sel_table_2, input$sel_column, input$rnm_column_to))
        showModal(modalDialog(
          title = "Success",
          "Column renamed successfully!",
          easyClose = TRUE
        ))
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Failed to rename column:", e$message),
          easyClose = TRUE
        ))
      })
    })

    observeEvent(input$delete_table, {
      req(input$sel_table_4)
      DBI::dbExecute(conn, paste0('DROP TABLE "', input$sel_table_4, '"'))
      # Update table_names reactive value
      table_names(DBI::dbListTables(conn))
      showModal(modalDialog(
        title = "Success",
        "Table deleted successfully!",
        easyClose = TRUE
      ))
    })
  })
}