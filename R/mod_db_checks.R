#' db_checks UI Function
#'
#' @description A shiny Module to identify errors in, and mismatches between, ICES SID and SAG databases
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_db_checks_ui <- function(id){
  ns <- NS(id)
  tagList(
      layout_column_wrap(
        fill = T,
        width = 1/2, heights_equal = "row",
        card(
          uiOutput(outputId = ns("year_selector")), 
          actionButton(inputId = ns("check"), label = "Check for mismatches",
                       class = "btn btn-primary"),
          value_box(
            title = "Stock Database issues",
            value = textOutput(ns("n_errors")),
            showcase = bs_icon("wrench"))
            ),
        card(height = "300px",
          card_header("Overview"),
          dataTableOutput(outputId = ns("EG_table")), full_screen = T
          )
      ),
      card(height = "600px",
        card_header("Detail"),
        DTOutput(outputId = ns("detail_table")), full_screen = T)
  )
}

#' db_checks Server Functions
#'
#' @noRd
#' @importFrom dplyr summarise n arrange
mod_db_checks_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$year_selector <- renderUI({
      years <- seq(year(Sys.Date()),year(Sys.Date())-7)
      if(month(Sys.Date()) <=5) {
        default_year <- years[2]
      } else {
        default_year <- years[1]
      }
      selectInput(inputId = ns("year"), label = "Select Assessment Year", choices = years, selected = default_year, multiple = F, width = "100%")
    })


    data <- reactive({
      issues <-check_stock_db_errors(year = as.numeric(input$year))
    }) %>%
      bindEvent(input$check)


    output$EG_table <- renderDT({
      req(!is.null(data()))
      
      eg_df <- data() %>% summarise(.by = ExpertGroup, Issues = n()) %>% arrange(desc(Issues))
      
      datatable(eg_df, options = list(pageLength = 20, 
                                      dom = "tip", 
                                      lengthMenu = c(5, 10, 15, 20)),
                    rownames = FALSE)
    })


    output$detail_table <- renderDT({
      req(!is.null(data()))
      
      detail_df <- select(data(), c(Stock, AssessmentKey, Database, Issue, ExpertGroup, YearOfLastAssessment, YearOfNextAssessment, AssessmentFrequency)) %>% 
        arrange(Stock)
      
      datatable(detail_df,filter = "top",
                options = list(pageLength = 20,
                               dom = "tip",
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
# mod_db_checks_ui("db_checks_1")

## To be copied in the server
# mod_db_checks_server("db_checks_1")
