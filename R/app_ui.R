#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib value_box
#' @importFrom bslib card_body
#' @importFrom bslib layout_column_wrap
#' @importFrom bslib bs_theme
#' @importFrom htmltools css
#' @importFrom bsicons bs_icon
#' @importFrom DT dataTableOutput
#' @importFrom DT DTOutput
#' @noRd
app_ui <- function(request) {
  tagList(

    golem_add_external_resources(),

    navbarPage(
      theme = bs_theme(bootswatch = "cyborg"),
      title = ("icesTEASD: Tool for Error Alignment of Stock Databases"),
      tabPanel("SID SAG checks",
               mod_SID_SAG_checks_ui("SID_SAG_checks_1")
      ),
      tabPanel("User checks", 
               mod_user_checks_ui("user_checks_1"))
    )
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
      app_title = "icesTEASD"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
