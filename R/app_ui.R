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
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(theme = bs_theme(bootswatch = "cyborg"),
      
      h1("icesTEASD: Tool for Error Alignment of Stock Databases"),
      fluidRow(
        shiny::uiOutput(outputId = "year_selector", style="padding-bottom:0px")),
      fluidRow(
        shiny::actionButton(inputId = "check", label = "Check for mismatches", class="btn btn-primary"), style= "padding-bottom:15px; padding-left:12px; padding-right:12px; padding-top:0px"),
      layout_column_wrap(
        width = NULL, height = 300, fill = FALSE,
        style = css(grid_template_columns = "1fr 2fr"),
        value_box(
          title = "Stock Database issues",
          value = textOutput("n_errors"),
          showcase = bs_icon("wrench")
          ), 
        # bslib::card_body(
        #   dataTableOutput(outputId = "PO_table")
        # )), 
        bslib::card_body(
          DTOutput(outputId = "stock_table"))
      )
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
