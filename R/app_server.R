#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom shiny renderUI reactive bindEvent req renderText selectInput
#' @importFrom DT renderDT renderDataTable datatable
#' @importFrom lubridate year month
#' @noRd
app_server <- function(input, output, session) {

  mod_SID_SAG_checks_server("SID_SAG_checks_1")
  mod_user_checks_server("user_checks_1")
  
}
