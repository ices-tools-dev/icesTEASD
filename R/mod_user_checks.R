#' user_checks UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_user_checks_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' user_checks Server Functions
#'
#' @importFrom shiny moduleServer
#' @noRd
mod_user_checks_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_user_checks_ui("user_checks_1")

## To be copied in the server
# mod_user_checks_server("user_checks_1")
