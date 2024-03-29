#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' 
#' 
#' @noRd
app_ui <- function(request) {
  
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage( # @importFrom shinydashboardPlus dashboardPage
      title = HTML(glue::glue('DbVieweR')),
		# Dashboard Page Setup ----------------------------------------------------
		# options = list(sidebarExpandOnHover = TRUE),
		skin = "purple",
		dashboardHeader(
			title = HTML(glue::glue('DbVieweR'))
		),

		# Dashboard Sidebar -------------------------------------------------------
		  dashboardSidebar(#collapsed = TRUE, 
		                   #minified = TRUE, 
                   div(textOutput("welcome"), style = "padding: 20px"),
                   sidebarMenu(
                     id = "sidebarmenu",  # Add this line
                     menuItem("View Tables", tabName = "view_table", icon = icon("search")),
                     #menuItem("Create Tables", tabName = "create_table", icon = icon("plus-square")),
                     menuItem("Update Tables", tabName = "update_table", icon = icon("exchange-alt")),
                     #menuItem("Insert Entries", tabName = "insert_value", icon = icon("edit")),
                     #modify entries menuItem("Delete Tables", tabName = "del_table", icon = icon("trash-alt")),
                     #Insert tbale form excel menuItem("Delete Tables", tabName = "del_table", icon = icon("trash-alt")),
                     #menuItem("Delete Tables", tabName = "del_table", icon = icon("trash-alt")),
                     menuItem("About", tabName = "about", icon = icon("info-circle"))
                   )
  ),
		# menuItem("Trading", icon = icon("chart-line", verify_fa = FALSE), startExpanded = F,
		# menuSubItem("Monitoring", tabName = "trades_tab", icon = icon("right-left")),
  
		# Dashboard Body ----------------------------------------------------------

    dashboardBody(
            title = HTML(glue::glue('DbVieweR')),
            shinyjs::useShinyjs(),
            tags$head(tags$style(".table{margin: 0 auto;}"),
                      tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                                  type="text/javascript")#,
                      #golem::add_js_file("returnClick.js")
            ),
           # tags$head(tags$style(HTML(
           #      '.myClass { 
           #      font-size: 20px;
           #      line-height: 50px;
           #      text-align: left;
           #      font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
           #      padding: 0 15px;
           #      overflow: hidden;
           #      color: white;
           #    }
           #  '))),
           #  tags$script(HTML('
           #  $(document).ready(function() {
           #    $("header").find("nav").append(\'<span class="myClass"> DbVieweR </span>\');
           #  })
           # ')),
      
            #shinyauthr::loginUI("login"),
            #uiOutput("user_table"),

            tabItems(
              # First Tab
              tabItem(
                tabName = 'view_table',
                mod_view_table_ui("view_table_1")
              ),
              # # Second Tab
              # tabItem(
              #   tabName = 'del_table',
              #   uiOutput("tab2UI")
              # ),
              # # Third Tab
              tabItem(
                tabName = 'update_table',
                mod_update_table_ui("update_table_1")
              ),
              # # fourth Tab
              # tabItem(
              #   tabName = 'create_table',
              #   uiOutput("tab4UI")
              # ),
              # # Fifth Tab
              # tabItem(
              #   tabName = 'insert_value',
              #   uiOutput("tab5UI")
              # ),
              # Sixth Tab
              tabItem(
                tabName = 'about',
                mod_about_ui("about_1")
              )
            )
          )
        )

			#tabItem(tabName = "trades_tab", tradesUI("trades_tab_ui_1"))

			)
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "DbVieweR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
