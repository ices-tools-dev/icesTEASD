#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom shiny renderUI reactive bindEvent req renderText selectInput
#' @importFrom DT renderDT renderDataTable datatable
#' @importFrom lubridate year month
#' @noRd
app_server <- function(input, output, session) {

  output$year_selector <- renderUI({
    years <- seq(year(Sys.Date()),year(Sys.Date())-2)
     if(month(Sys.Date()) <=5) {
       default_year <- years[2]
     } else {
       default_year <- years[1]
     }
    selectInput(inputId = "year", label = "Select Assessment Year", choices = years, selected = default_year, multiple = F)
  })

  data <- reactive({
    issues <- check_stock_db_errors(year = input$year)
  }) %>%
    bindEvent(input$check)


  output$PO_table <- renderDataTable({
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

  mod_user_checks_server("user_checks_1")
}
