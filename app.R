# the necessary packages
require(pacman)

#lazy load or install necessary packages
suppressPackageStartupMessages(
  pacman::p_load(
    DBI, dplyr, RSQLite, DT, shiny, shinydashboard,
    shinyjs, shinythemes, shinyWidgets, lubridate,
    shinyauthr, shinyFeedback, sodium, DBI, openxlsx
    )
  )

source("globals.R")

# connect to, or setup and connect to local SQLite db

if (file.exists("auth_db.db")) {
  db <- dbConnect(SQLite(), "auth_db.db")
} else {
  db <- dbConnect(SQLite(), "auth_db.db")
  dbCreateTable(conn, "sessionids", c(user = "TEXT", sessionid = "TEXT", login_time = "TEXT"))
}



# connect to the dummy database
conn <- dbConnect(SQLite(), 'dummy_database.db')
# TODO make this automatic
#tables <- DBI::dbListTables(db)


###############################################
# define the ui function
###############################################
ui <- dashboardPage(
  title="Database Management Platform",

  dashboardHeader(
    title = span("Database Management Platform", style = "font-size: 20px"),
    titleWidth = 300,
    tags$li(class = "dropdown", style = "padding: 8px;", actionButton("refresh", "refresh")),
    tags$li(class = "dropdown", 
            tags$a(icon("github"), 
                   href = "https://github.com/munoztd0/DbVieweR/blob/main/app.R",
                   title = "See the code on github")),
    tags$li(class = "dropdown", style = "padding: 8px;",
            shinyauthr::logoutUI("logout"))
    
  ),

  
  dashboardSidebar(collapsed = TRUE, 
                   div(textOutput("welcome"), style = "padding: 20px"),
                   sidebarMenu(
                     menuItem("View Tables", tabName = "view_table", icon = icon("search")),
                     menuItem("Create Tables", tabName = "create_table", icon = icon("plus-square")),
                     menuItem("Update Tables", tabName = "update_table", icon = icon("exchange-alt")),
                     menuItem("Insert Entries", tabName = "insert_value", icon = icon("edit")),
                     #modify entries menuItem("Delete Tables", tabName = "del_table", icon = icon("trash-alt")),
                     #Insert tbale form excel menuItem("Delete Tables", tabName = "del_table", icon = icon("trash-alt")),
                     menuItem("Delete Tables", tabName = "del_table", icon = icon("trash-alt")),
                     menuItem("About", tabName = "about", icon = icon("info-circle"))
                   )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$style(".table{margin: 0 auto;}"),
              tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                          type="text/javascript"),
              includeScript("returnClick.js")
    ),
    shinyauthr::loginUI("login"),
    uiOutput("user_table"),

    tabItems(
      # First Tab
      tabItem(
        tabName = 'view_table',
        uiOutput("tab1UI")
      ),
      # Second Tab
      tabItem(
        tabName = 'del_table',
        uiOutput("tab2UI")
      ),
      # Third Tab
      tabItem(
        tabName = 'update_table',
        uiOutput("tab3UI")
      ),
      # fourth Tab
      tabItem(
        tabName = 'create_table',
        uiOutput("tab4UI")
      ),
      # Fifth Tab
      tabItem(
        tabName = 'insert_value',
        uiOutput("tab5UI")
      ),
      # Sixth Tab
      tabItem(
        tabName = 'about',
        uiOutput("tab6UI")
      )
    )
  )
)

###############################################
# define the server function
###############################################

server <- function(input, output, session) {

  backgroundchange <- reactive({
      invalidateLater(1000, session)

      runif(1)
    })
  
  # call login module supplying data frame, user and password cols
  # and reactive trigger
  credentials <- shinyauthr::loginServer("login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password_hash,
                            sessionid_col = sessionid,
                            sodium_hashed = TRUE,  
                            cookie_getter = get_sessionids_from_db,
                            cookie_setter = add_sessionid_to_db,
                            log_out = reactive(logout_init()))
  
    # call the logout module with reactive trigger to hide/show
      logout_init <- shinyauthr::logoutServer(
        id = "logout",
        active = reactive(credentials()$user_auth)
      )
  
  # Add or remove a CSS class from an HTML element
  # Here sidebar-collapse
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  
  # Show the sample login info
  output$user_table <- renderUI({
    # only show pre-login
    if(credentials()$user_auth) return(NULL)
    fluidRow(column(6,
                    p("Please use the usernames and passwords provided below to test this database management platform. 
                      There are two kinds of accounts: admin and manager. 
                      The manager account allows one to alter tables, columns, and records, and delete tables. 
                      The admin account allows one to do everything above except for deleting tables.", 
                      class = "text-center", style = "font-size: 15px;"),
                    br(),
                    renderTable({user_base[, -3]}), offset = 3
    )
    )
  })
  
  # pulls out the user information returned from login module
  user_info <- reactive({credentials()$info})
  
  # menu welcome info
  output$welcome <- renderText({
    req(credentials()$user_auth)
    paste0("Welcome ",{user_info()$permissions},"!")
  })
  
  ############# Tab 1: View Table
  output$tab1UI <- renderUI({
    req(credentials()$user_auth)

    Listener1 <- input$refresh
    isolate(backgroundchange())


    box(width = NULL, status = "primary",
        wellPanel(
          sidebarLayout(
            sidebarPanel(
              selectInput(
                inputId = 'sel_table_1',
                label = 'Tables in Database',
                choices = dbListTables(db)
              )
              ),
            mainPanel(
              h4(strong("Table Preview")),
              br(),
              dataTableOutput(
                outputId = 'sel_table_view')
            )
          )
        )
    )
  })
  
  # show table
  output$sel_table_view <- renderDataTable(
    dbGetQuery(
      conn = db,
      statement = paste0('SELECT * from ',input$sel_table_1)
    )
  )
  
  ############# Tab 2: Create Table 
  # Type in the table name and the column names
  output$tab4UI <- renderUI({
    req(credentials()$user_auth)
    box(width = NULL, status = "primary",
        textInput(inputId = "table_name", label = "Table name"),
        numericInput(inputId = "ncols", label = "Number of columns", NULL, min = 0,max = 10),
        uiOutput(outputId = "cols"),
        actionButton(inputId = "create_table", label = "Create table", class = "btn-info",
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  })
  
  # Type in the column names and the column types
  output$cols <- renderUI({
    req(input$ncols>=1)
    cols <- vector("list", input$ncols)
    for (i in seq_len(input$ncols)) {
      cols[[i]] <- box(
        title = paste("Column", i), width = 6, solidHeader = TRUE, status = "primary",
        textInput(inputId = paste0("colName", i), label = "Column name"),
        selectInput(inputId = paste0("colType", i), label = "Column type", 
                    choices = c("NUMERIC", "VARCHAR(255)","BOOLEAN")
        )
      )
    }
    cols
  })
  
  # Create!
  observeEvent(input$create_table, {
    
    # in case the table name is null or has existed
    if (tolower(input$table_name) %in% tolower(dbListTables(db)) |
        !isTruthy(input$table_name) |
        grepl("^[a-zA-Z_][a-zA-Z0-9_]*$",input$table_name) == FALSE) {
      showModal(modalDialog(
        title = "Invalid table name",
        "You get this message possibly because:
         1) the table already exists;
         2) the table name is blank;
         or 3) this is an invalid table name.",
        footer = modalButton("OK"), easyClose = TRUE ) )
      return()
    }
    
    # in case the input ncols blank
    if (!isTruthy(input$ncols)) {
      showModal(modalDialog(
        title = "Invalid table name",
        "Please type in the right column number.",
        footer = modalButton("OK"), easyClose = TRUE ) )
    }
    
    else if (input$ncols < 1) {
      showModal(modalDialog(
        title = "No columns",
        "Each table must have one or more columns.",
        footer = modalButton("OK"), easyClose = TRUE
      )) 
    }  
    
    else {
      
      # gather all the colnames into a list
      col_names_list = list()
      for (i in seq_len(input$ncols)) {
        col_names_list <- c(col_names_list,input[[paste0("colName", i)]])
      }
      
      # in case there are column with no names/duplicate names/informal signs/in sqlite keywords
      if ( any(col_names_list == '') | 
           sum(duplicated(col_names_list)) > 0 |
           any(grepl("^[a-zA-Z_][a-zA-Z0-9_]*$",col_names_list) == FALSE) |
           any(tolower(col_names_list) %in% sqlite_kw_lo) ) {
        showModal(modalDialog(
          title = "Invalid column name",
          "You get this message possibly because: 
           1) one or more fields are blank;
           2) one or more fields contain invalid SQLite column name(s); 
           3) there are duplicate column names;
           or 4) one or more fields conflict with a SQLite keyword.",
          footer = modalButton("OK"), easyClose = TRUE
        ) )
        return()
      }
      
      # compile query
      query <- paste0('CREATE TABLE ',input$table_name,' (')
      for (i in seq_len(input$ncols)) { 
        query <- paste0(query,input[[paste0("colName", i)]],' ',input[[paste0("colType", i)]],',')
      }
      query <- paste0(str_sub(query,1,-2),')')
      dbGetQuery(
        conn = db,
        statement = query )
      # if succuess, after create table, update the list of tables in tab2,3,5 and clear the input box
      # if not, no update date
      updateNumericInput(session, "ncols", value = '1')
      updateTextInput(session, "table_name", value = '')
      for (sel_input in c('sel_table_2','sel_table_3','sel_table_3_i','sel_table_3_ii')) {
        updateSelectInput(session, sel_input, 
                          choices = setdiff(dbListTables(db),
                                            c("custs","order_items","orders",
                                              "prods","prods_i","stores")))
      }
      updateSelectInput(session, 'sel_table_1', choices = dbListTables(db))
      updateSelectInput(session, 'sel_table_5', 
                        choices = setdiff(dbListTables(db),
                                          c("custs","order_items",
                                            "prods","prods_i","stores")))
      showModal(modalDialog(
        title = "Success",
        "The table has been successfully created.",
        footer = modalButton("OK"), easyClose = TRUE ) )
    }
  }
  )
  
  ############# Tab 3: Update Table (rename table, rename column, add column)
  output$tab3UI <- renderUI({
    req(credentials()$user_auth)
    fluidPage(
      fluidRow( 
        box(width = 12,
            #solidHeader = FALSE,
            collapsible = TRUE,
            div(style = "height: 15px; background-color: white;"),
            title = "Note:",
            "You can only alter tables that are created in the ",
            tags$i("Create Tables "),
            "tab. If you find the dropdown menu empty, this means currently no table can be altered.",
            style = "font-size: 15px;")
         ),
      fluidRow(
        # rename table
        box(title = 'Rename Table', width = 4, solidHeader = TRUE, status = "primary",
            # tags$i("You can only rename tables that are created in "Create Tables` tab. If you find the dropdown menu is empty, this means currently no table can be altered."),
           
            selectInput(
              inputId = 'sel_table_3_ii',
              label = 'Select Table:',
              choices = setdiff(dbListTables(db),
                                c("custs","order_items","orders",
                                  "prods","prods_i","stores")),
              selected = 'test_table'
            ),
            wellPanel(
              textInput(inputId = "rnm_table_to",
                        label = "Rename To:"),
              actionButton(inputId = "rename_table",
                           label = "Rename Table", 
                           # class = "pull-right btn-info",
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4; display: right-align" )
                     )
           ),
              
            
      
        # rename colunm
        
        box(title = 'Rename Column', width = 4, solidHeader = TRUE, status = "primary",
            #tags$i("You can only rename columns in the tables that are created in `Create Tables` tab. If you find the dropdown menu empty, this means currently no table can be altered."),
           
            selectInput(
              inputId = 'sel_table_3',
              label = 'Select Table:',
              choices = setdiff(dbListTables(db),
                                c("custs","order_items","orders",
                                  "prods","prods_i","stores")),
              selected = 'test_table'
            ),
            wellPanel(
              selectInput(
                inputId = 'sel_col_3',
                label = 'Select Column:',
                choices = NULL),
              textInput(inputId = "rnm_col_to",
                        label = "Rename the Column:"),
              actionButton(inputId = "rename_col",
                           label = "Rename Column", 
                           #class = "pull-right btn-info",
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              )
            )
          ),
              
      
      # add column
     
      box(title = 'Add Column', width = 4, solidHeader = TRUE, status = "primary",
          # tags$i("You can only add columns in the tables that are created in `Create Tables` tab. If you find the dropdown menu is empty, this means currently no table can be altered."),
          
           selectInput(
             inputId = 'sel_table_3_i',
             label = 'Select Table:',
             choices = setdiff(dbListTables(db),
                               c("custs","order_items","orders",
                                 "prods","prods_i","stores")),
             selected = 'test_table'
           ),
           wellPanel(
             textInput(inputId = 'add_col_name', label = "Add Column"),
             selectInput(inputId = "add_col_type", label = "Add Column Type", 
                         choices = c("NUMERIC","VARCHAR(255)","BOOLEAN")
             ),
             actionButton(inputId = "add_col",
                          label = "Add Column",
                          #class = "pull-right btn-info",
                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
           )
      )
      )
      )
    
  })
  
  # show column names
  observeEvent(input$sel_table_3,{
    req(isTruthy(input$sel_table_3))
      d <- dbGetQuery(
        conn = db,
        statement = paste0('SELECT * from ',input$sel_table_3)
      )
      updateSelectInput(session, "sel_col_3", choices = colnames(d))
  })
  
  # Rename Column!
  observeEvent(input$rename_col, {
    req(isTruthy(input$sel_col_3))
    d <- dbGetQuery(
      conn = db,
      statement = paste0('Select * from ',input$sel_table_3)
    )
    # in case the column name already exists
      if (input$rnm_col_to %in% colnames(d) | 
        !isTruthy(input$rnm_col_to)| 
        grepl("^[a-zA-Z_][a-zA-Z0-9_]*$",input$rnm_col_to) == FALSE |
        tolower(input$rnm_col_to) %in% sqlite_kw_lo )  {
      showModal(modalDialog(
        title = "Invalid column name",
        "You get this message possibly because: 
                       1) the column name already exists;
                       2) the field is blank;
                       3) this is an invalid SQLite column name;
                       or 4) the field name conflicts with a SQLite keyword.",
        footer = modalButton("OK"), easyClose = TRUE ) )
    } else {
      dbGetQuery(
        conn = db,
        statement = paste0('ALTER TABLE ',input$sel_table_3,
                           ' RENAME COLUMN ',input$sel_col_3,
                           ' TO ',input$rnm_col_to)
      )
      # after rename column, clear the input box
      updateTextInput(session, "rnm_col_to", value = '')
      # after rename column, update table/colunm select range
      updateSelectInput(session, "sel_table_3", 
                        choices = setdiff(dbListTables(db),
                                  c("custs","order_items","orders",
                                    "prods","prods_i","stores")))
      d1 <- dbGetQuery(
        conn = db,
        statement = paste0('Select * from ',input$sel_table_3)
      )
      updateSelectInput(session, "sel_col_3", choices = colnames(d1))
      showModal(modalDialog(
        title = "Success",
        "The column has been successfully renamed.",
        footer = modalButton("OK"), easyClose = TRUE ) )
    }
  }
  )
  
  # Add Column! 
  observeEvent(input$add_col, {
    req(isTruthy(input$sel_table_3_i))
    d <- dbGetQuery(
      conn = db,
      statement = paste0('Select * from ',input$sel_table_3_i)
    )
    # in case the col name already exists
    if ( !isTruthy(input$add_col_name) | 
         input$add_col_name %in% colnames(d) |
         grepl("^[a-zA-Z_][a-zA-Z0-9_]*$",input$add_col_name) == FALSE |
         tolower(input$add_col_name) %in% sqlite_kw_lo ) 
    {
      showModal(modalDialog(
        title = "Invalid column name",
        "You get this message possibly because: 
                       1) the column name already exists;
                       2) the field is blank;
                       3) this is an invalid SQLite column name;
                       or 4) the field name conflicts with a SQLite keyword.",
        footer = modalButton("OK"), easyClose = TRUE
      ) )
    } else {
      dbGetQuery(
        conn = db,
        statement = paste0('ALTER TABLE ',input$sel_table_3_i,
                           ' ADD COLUMN ',input$add_col_name,
                           ' ',input$add_col_type)
      )
      # after add column, clear text input
      updateTextInput(session, "add_col_name", value = '')
      # after add column, update colunm select range
      updateSelectInput(session, "sel_table_3",
                        choices = setdiff(dbListTables(db),
                                          c("custs","order_items","orders",
                                            "prods","prods_i","stores")))
      updateSelectInput(session, "sel_col_3", choices = colnames(d))
      showModal(modalDialog(
        title = "Success",
        "The column has been successfully added.",
        footer = modalButton("OK"), easyClose = TRUE ) )
    }
  }
  )
  
  # Rename Table! 
  observeEvent(input$rename_table, {
               req(isTruthy(input$sel_table_3_ii))
                 # in case duplicates/blank/invalid
                  if (tolower(input$rnm_table_to) %in% tolower(dbListTables(db)) | 
                     !isTruthy(input$rnm_table_to) | 
                     grepl("^[a-zA-Z_][a-zA-Z0-9_]*$",input$rnm_table_to) == FALSE) { 
                     showModal(modalDialog(
                     title = "Invalid table name",
                     "You get this message possibly because: 
                       1) the table already exists;
                       2) the table name is blank;
                       or 3) this is an invalid table name.",
                     footer = modalButton("OK"), easyClose = TRUE ) )
                 } else {
                   dbGetQuery(
                     conn = db,
                     statement = paste0('ALTER TABLE ',input$sel_table_3_ii,
                                        ' RENAME TO ',input$rnm_table_to)
                   )
                   
                   # after rename table, clear the text input
                   updateTextInput(session, "rnm_table_to", value = '')
                   # after rename table, update the list of tables in tab 2,3,5 
                   for (sel_input in c('sel_table_2','sel_table_3','sel_table_3_i','sel_table_3_ii')) {
                     updateSelectInput(session, sel_input, 
                                       choices = setdiff(dbListTables(db),
                                                         c("custs","order_items","orders",
                                                           "prods","prods_i","stores")))
                   }
                   updateSelectInput(session, 'sel_table_1', choices = dbListTables(db))
                   updateSelectInput(session, 'sel_table_5', 
                                     choices = setdiff(dbListTables(db),
                                                       c("custs","order_items",
                                                         "prods","prods_i","stores")))
                   showModal(modalDialog(
                     title = "Success",
                     "The table has been successfully renamed.",
                     footer = modalButton("OK"), easyClose = TRUE ) )
                 }
               }
  )
  
  
  ############# Tab 4: Insert Values
  # select the table to insert values
  output$tab5UI <- renderUI({
    req(credentials()$user_auth)
    box(width = NULL, status = "primary",
        selectInput(
          inputId = 'sel_table_5',
          label = 'Select Table:',
          choices = setdiff(dbListTables(db),
                            c("custs","order_items",
                              "prods","prods_i","stores")),
          selected = 'orders'
        ),
        # show each colnames names
        uiOutput("values"),
        actionButton(inputId = "insert_value", label = "Insert Value", class = "pull-right btn-info",
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    )
  })
  
  # Type in the corresponding values names 
  output$values <- renderUI({
    values <- list()
    d <- dbGetQuery(
      conn = db,
      statement = paste0('SELECT * from ',input$sel_table_5)
    )
    typ <- dbGetQuery(
      conn = conn, statement = paste0('PRAGMA table_info(',input$sel_table_5,')')
    )
    for (col in colnames(d)) {
      typ_i = typ$type[typ$name==col]
      values[[col]] <- box(
        title = paste0(as.character(col),' (',typ_i,')'), 
        width = 6, solidHeader = TRUE, status = "primary",
        
        if (typ_i == 'BOOLEAN') {radioButtons(inputId = paste0("value_", col), label = 'Value',
                                              c("TRUE","FALSE") )}
        else if (typ_i == 'NUMERIC' | typ_i == 'FLOAT' |
                 typ_i == 'INTEGER' | typ_i == 'NUM' ) 
        {numericInput(inputId = paste0("value_", col), label = 'Value',value = 0)} 
        else if (typ_i == 'DATE') {dateInput(inputId = paste0("value_", col),
                                             label = "Value",
                                             value = "2020-12-01") }
        else {tagList(useShinyFeedback(),
                      textInput(inputId = paste0("value_", col), label = 'Value'))}
      )
    }
    values
  })
  
  # check ord_id is null
  observeEvent(input$value_ord_id, {
    if (!isTruthy(input$value_ord_id)) {
      showFeedbackWarning(
        inputId = "value_ord_id",
        text = "Field ord_id cannot be blank."
      )  
    } else {
      hideFeedback("value_ord_id")
    }
  }
  )
  
  # check store_id num of char
  observeEvent(input$value_store_id, {
    if (nchar(input$value_store_id) > 5) {
      showFeedbackWarning(
        inputId = "value_store_id",
        text = "Maximum length is 5."
      )  
    } else {
      hideFeedback("value_store_id")
    }
  }
  )
  
  # Insert!
  observeEvent(input$insert_value, {
    d <- dbGetQuery(
      conn = db,
      statement = paste0('SELECT * from ',input$sel_table_5)
    )
    l <- dbGetQuery(
      conn = db,
      statement = 'SELECT ord_id from orders'
    )
    
    req(nchar(input$value_store_id) <= 5) 
    
    if (input$value_ord_id %in% l$ord_id) { 
      showModal(modalDialog(
        title = "UNIQUE constraint failed",
        "Primary key values have to be unique.",
        footer = modalButton("OK"), easyClose = TRUE ) )
    } else {
      
      # compile query 
      query <- paste0('INSERT INTO ',input$sel_table_5,' VALUES (')
      for (col in colnames(d)) {
        query <- paste0(query,'"',input[[paste0("value_", col)]],'",') }
      query <- paste0(str_sub(query,1,-2),')')
      dbGetQuery(
        conn = db,
        statement = query )
      
      # update input 
      for (col in colnames(d)) {
        updateTextInput(session, paste0("value_",col), value = '')
      }
      for (sel_input in c('sel_table_2','sel_table_3','sel_table_3_i','sel_table_3_ii')) {
        updateSelectInput(session, sel_input, 
                          choices = setdiff(dbListTables(db),
                                            c("custs","order_items","orders",
                                              "prods","prods_i","stores")))
      }
      updateSelectInput(session, 'sel_table_1', choices = dbListTables(db))
      updateSelectInput(session, 'sel_table_5', 
                        choices = setdiff(dbListTables(db),
                                          c("custs","order_items",
                                            "prods","prods_i","stores")))
      showModal(modalDialog(
        title = "Success",
        "The values have been successfully inserted.",
        footer = modalButton("OK"), easyClose = TRUE ) )
    }
  }  
  )
  
  ############# Tab 5: Delete Table
  output$tab2UI <- renderUI({
    req(credentials()$user_auth)
    
    box(width = NULL, status = "primary",
        wellPanel(
          sidebarLayout(
            sidebarPanel(
              selectInput(
                inputId = 'sel_table_2',
                label = 'Tables in Database',
                choices = setdiff(dbListTables(db),
                                  c("custs","order_items","orders",
                                    "prods","prods_i","stores")),
                selected = 'test_table'
              ),
              actionButton(inputId = "del_tab", 
                           label = "Delete the Table",
                           style="color: #fff; background-color: #C11B17"
              ),
              hr(),
              tags$i("Note: You are only allowed to delete the newly created tables. Original tables cannot be deleted."),
              tags$style(type='text/css', "button#del_tab {margin-left: 60%;}")
              
            ),
            mainPanel(
              h4(strong("Table Preview")),
              br(),
              dataTableOutput(
                outputId = 'sel_table')
              
            )
          )
        )
    )
  })
  
  # show table
  output$sel_table <- renderDataTable(
    dbGetQuery(
      conn = db,
      statement = paste0('SELECT * from ',input$sel_table_2)
    )
  )
  
  # check auth and then confirm delete
  observeEvent(input$del_tab, 
               if (user_info()$permissions == "admin") {
                 showModal(
                   modalDialog(
                     title = "Access Denied",
                     "Please ask the manager to give you the permission to delete tables.",
                     footer = modalButton("OK"), easyClose = TRUE
                   )
                 )
               }
               else {
                 # check if original tables, if so then prompt
                 confirmSweetAlert(
                   session = session, inputId = "confirmation", type = "warning",
                   title = "Are you sure you want to delete the table?", 
                   text = "This action cannot be undone.", danger_mode = TRUE
                 )
               })
  
  # if confirmed then delete!
  observeEvent(input$confirmation, {
    if (isTRUE(input$confirmation)) {
      dbGetQuery(
        conn = db,
        statement = 
          paste0('DROP TABLE ',input$sel_table_2)  
      )
      for (sel_input in c('sel_table_2','sel_table_3','sel_table_3_i','sel_table_3_ii')) {
        updateSelectInput(session, sel_input, 
                          choices = setdiff(dbListTables(db),
                                            c("custs","order_items","orders",
                                              "prods","prods_i","stores")))
      }
      updateSelectInput(session, 'sel_table_1', choices = dbListTables(db))
      updateSelectInput(session, 'sel_table_5', 
                        choices = setdiff(dbListTables(db),
                                          c("custs","order_items",
                                            "prods","prods_i","stores")))
      showModal(
        modalDialog(
          title = "Success",
          "The table has been successfully deleted.",
          footer = modalButton("OK"), easyClose = TRUE
        )
      )
    }
  } 
  )
  
  
  ############# Tab 6: About
  output$tab6UI <- renderUI({
    req(credentials()$user_auth)
    
    box(title = 'About this app',width = NULL, status = "primary",solidHeader = TRUE,
        
        "This Shiny app is a prototype of a database management system, featuring a variety of functions. 
          This app addresses the needs in back-end database management with a clean and easy to use UI.",
        br(),
        "Check the code on Github:",
        tags$head(tags$style(HTML("a {color: blue}"))),
        tags$a(icon("github"), 
               href = "https://github.com/munoztd0/DBMS/blob/main/app.R",
               title = "See the code on github"),
        
        br(),
        br(),
        br(),
        br(),
        h5(strong("Developer")),
        img(src = 'https://avatars.githubusercontent.com/u/43644805?s=400&u=41bc00f6ee310ed215298c9af27ed53e9b3e1a60&v=4', height = 150, align = "right"),
        
        "David Munoz Tord, Fullstack Data Scientist / R Developer",
        tags$a(icon("linkedin"), 
               href = "https://www.linkedin.com/in/david-munoz-tord-409639150",
               title = "See the code on github"),
        br(),
        p("A data enthusiast with steady ambition and restless curiosity.")
        
        
    )  
    
  })
  
  
  
}

# when exiting app, disconnect from the apple database
onStop(
  function()
  {
    dbDisconnect(db)
  }
)

# execute the Shiny app
shinyApp(ui, server)

