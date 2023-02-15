#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect
#' 

run_app <- function(#user = Sys.getenv("data_hub_uid"),
                    #password = Sys.getenv("data_hub_pwd"),
                    onStart = NULL,
                    options = list(),
                    enableBookmarking = NULL,
                    uiPattern = "/",
                    ...) {
  with_golem_options(
    app = shinyApp(
      ui = shinymanager::secure_app(app_ui, head_auth = tags$script(inactivity), theme = shinythemes::shinytheme("flatly")),
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
      "conn_SQL_Lite" =  dbConnect(SQLite(), 'dummy_database.db'))
  )
}
