#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DT renderDT
#' @importFrom DT renderDataTable
#' @importFrom lubridate year
#' @noRd
app_server <- function(input, output, session) {

  mod_SID_SAG_checks_server("SID_SAG_checks_1")
  mod_user_checks_server("user_checks_1")
  
}
