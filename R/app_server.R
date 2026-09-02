#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {

  mod_db_checks_server("db_checks_1")
  mod_user_checks_server("user_checks_1")
  
}