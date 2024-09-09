#' Create Report UI Function
#'
#' @description A shiny Module for creating reports using DataExplorer.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_create_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Create Report", 
        status = "primary", 
        solidHeader = TRUE, 
        width = 12,
        actionButton(ns("generate_report"), "Generate Report", class = "btn-primary"),
        br(),
        br(),
        uiOutput(ns("report_iframe"))
      )
    )
  )
}#' Create Report UI Function
#'
#' @description A shiny Module for creating reports using DataExplorer.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_create_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Create Report", 
        status = "primary", 
        solidHeader = TRUE, 
        width = 12,
        actionButton(ns("generate_report"), "Generate Report", class = "btn-primary"),
        br(),
        br(),
        uiOutput(ns("report_iframe"))
      )
    )
  )
}

