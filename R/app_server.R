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

  output$year_selector <- renderUI({
    years <- seq(lubridate::year(Sys.Date()),lubridate::year(Sys.Date())-2)
     if(lubridate::month(Sys.Date()) <=5) {
       default_year <- years[2]
     } else {
       default_year <- years[1]
     }
    shiny::selectInput(inputId = "year", label = "Select Assessment Year", choices = years, selected = default_year, multiple = F)
  })

  data <- reactive({
    issues <-check_stock_db_errors(year = input$year)
  }) %>%
    bindEvent(input$check)


  output$PO_table <- DT::renderDataTable({
    req(!is.null(data()))
    DT::datatable(data.frame(PO =  1:4), options = list(dom="t"),
                  rownames = FALSE)

  })

  output$stock_table <- DT::renderDT({
    req(!is.null(data()))
    DT::datatable(data(), options = list(pageLength = 10,
                                    lengthMenu = c(5, 10, 15, 20)),
                  rownames = FALSE)
  })


  output$n_errors <- renderText({
    req(!is.null(data()))
    nrow(data())
  })

  mod_user_checks_server("user_checks_1")
}
