#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    box(title = 'About this app',width = NULL, status = "primary",solidHeader = F,
        
        "This Shiny app is a prototype of a database management system, featuring a variety of functions. 
          This app addresses the needs in back-end database management with a clean and easy to use UI.",
        br(),
        "Check the code on Github:",
        tags$head(tags$style(HTML("a {color: purple}"))),
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
 
  )
}
    
#' about Server Functions
#'
#' @noRd 
# mod_about_server <- function(id){
#   moduleServer( id, function(input, output, session){
#     ns <- session$ns
 
#   })
# }
    
## To be copied in the UI
# mod_about_ui("about_1")
    
## To be copied in the server
# mod_about_server("about_1")
