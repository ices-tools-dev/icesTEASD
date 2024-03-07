#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom shiny tagList navbarPage tabPanel fluidRow uiOutput textOutput actionButton
#' @importFrom bslib value_box card card_header card_body layout_column_wrap bs_theme 
#' @importFrom htmltools css h1 tags
#' @importFrom bsicons bs_icon
#' @importFrom DT dataTableOutput DTOutput
#' @importFrom desc desc_get_version
#'
#' @noRd
app_ui <- function(request) {
  tagList(

    golem_add_external_resources(),

    navbarPage(
      theme = bs_theme(bootswatch = "cyborg"),
      title = paste0("icesTEASD: Tool for Error Alignment of Stock Databases, v", desc_get_version()),
      tabPanel("Database checks",
               mod_db_checks_ui("db_checks_1")
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
