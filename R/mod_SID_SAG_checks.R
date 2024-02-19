#' SID_SAG_checks UI Function
#'
#' @description A shiny Module to identify errors in, and mismatches between, ICES SID and SAG databases
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SID_SAG_checks_ui <- function(id){
  ns <- NS(id)
  tagList(
      fluidRow(
        uiOutput(outputId = ns("year_selector"), style = "padding-bottom:0px")
      ),
      fluidRow(
        actionButton(inputId = ns("check"), label = "Check for mismatches", class = "btn btn-primary"),
        style = "padding-bottom:15px; padding-left:12px; padding-right:12px; padding-top:0px"
      ),
      layout_column_wrap(
        width = NULL, height = 300, fill = FALSE,
        style = css(grid_template_columns = "1fr 2fr"),
        value_box(
          title = "Stock Database issues",
          value = textOutput(ns("n_errors")),
          showcase = bs_icon("wrench")
        ),
        # bslib::card_body(
        #   dataTableOutput(outputId = "EG_table")
        # )),
        card_body(
          DTOutput(outputId = ns("stock_table"))
        )
      )
  )
}

#' SID_SAG_checks Server Functions
#'
#' @noRd
mod_SID_SAG_checks_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$year_selector <- renderUI({
      years <- seq(year(Sys.Date()),year(Sys.Date())-7)
      if(month(Sys.Date()) <=5) {
        default_year <- years[2]
      } else {
        default_year <- years[1]
      }
      selectInput(inputId = ns("year"), label = "Select Assessment Year", choices = years, selected = default_year, multiple = F)
    })


    data <- reactive({
      issues <-check_stock_db_errors(year = input$year)
    }) %>%
      bindEvent(input$check)


    output$EG_table <- renderDataTable({
      req(!is.null(data()))
      datatable(data.frame(PO =  1:4), options = list(dom="t"),
                    rownames = FALSE)
    })


    output$stock_table <- renderDT({
      req(!is.null(data()))
      datatable(data(), options = list(pageLength = 10,
                                           lengthMenu = c(5, 10, 15, 20)),
                    rownames = FALSE)
    })


    output$n_errors <- renderText({
      req(!is.null(data()))
      nrow(data())
    })
  })
}

## To be copied in the UI
# mod_SID_SAG_checks_ui("SID_SAG_checks_1")

## To be copied in the server
# mod_SID_SAG_checks_server("SID_SAG_checks_1")
