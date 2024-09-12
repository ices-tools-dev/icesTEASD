#' db_checks UI Function
#'
#' @description A shiny Module to identify errors in, and mismatches between, ICES SID and SAG databases
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinycssloaders withSpinner
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
          layout_column_wrap(
            fill = T,
            width = 1/3,  
              value_box(
                title = "SID",
                value = textOutput(ns("n_SID")),
                showcase = bs_icon("wrench")),
              value_box(
                title = "SAG",
                value = textOutput(ns("n_SAG")),
                showcase = NULL),
              value_box(
                title = "ASD",
                value = textOutput(ns("n_ASD")),
                showcase = NULL),
            ),
          numericInput(ns("preview_days"), "Advice release 'preview' (days)", min = 0, max = 365, value = 7)),
        card(height = "300px",
          card_header("Overview"),
          dataTableOutput(outputId = ns("EG_table")), full_screen = T)
      ),
      layout_column_wrap(
        fill = T,
        width = 1/3,  
        card(height = "600px",
               card_header("SID"),
               DTOutput(outputId = ns("SID")), full_screen = T),
        card(height = "600px",
              card_header("SAG"),
              withSpinner(DTOutput(outputId = ns("SAG")), type = 8), full_screen = T),
        card(height = "600px",
               card_header("ASD"),
               DTOutput(outputId = ns("ASD")), full_screen = T)
    
  )
  )
}

#' db_checks Server Functions
#'
#' @noRd
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
      selectInput(inputId = ns("year"), label = "Select year of advice validity", choices = years, selected = default_year, multiple = F, width = "100%")
    })


    stock_data <- reactive({
    
      data <- get_stock_data(as.numeric(input$year))
      
    }) %>% bindEvent(input$check)

    
    checks_data <- reactive({
      req(!is.null(stock_data()))
      issues <-check_stock_db_errors(stock_data()$SID_data,
                                     stock_data()$SAG_data,
                                     stock_data()$ASD_data,
                                     year = as.numeric(input$year),
                                     preview_days = as.numeric(input$preview_days))
    })

    output$EG_table <- renderDT({
      req(!is.null(checks_data()))
      
      datatable(checks_data()$issue_count, options = list(pageLength = 20, 
                                      dom = "tip", 
                                      lengthMenu = c(5, 10, 15, 20)),
                    rownames = FALSE)
    })


    output$SID <- renderDT({
       req(!is.null(checks_data()$SID))
      detail_df <- select(checks_data()$SID, Stock, Issue, "Expert Group" = ExpertGroup, "Year Of Last Assessment" = YearOfLastAssessment, "Year Of Next Assessment" = YearOfNextAssessment)
      
      datatable(detail_df,filter = "top",
                            options = list(pageLength = 20,
                                           dom = "tip",
                                           lengthMenu = c(5, 10, 15, 20)),
                                rownames = FALSE)
    })
    
    output$SAG <- renderDT({
       req(!is.null(checks_data()$SAG))

      detail_df <- select(checks_data()$SAG, Stock, 
                          "Assessment Key" = AssessmentKey.x,
                          Issue,
                          "Expert Group" = ExpertGroup,
                          "Year Of Last Assessment" = YearOfLastAssessment)
      
      datatable(detail_df,filter = "top",
                            options = list(pageLength = 20,
                                           dom = "tip",
                                           lengthMenu = c(5, 10, 15, 20)),
                                rownames = FALSE)
    })
    
    output$ASD <- renderDT({
       req(!is.null(checks_data()$ASD))

      detail_df <- select(checks_data()$ASD, 
                          Stock, 
                          "Assessment Key" = AssessmentKey.x,
                          Issue, 
                          "Expert Group" = ExpertGroup, 
                          "Year Of Last Assessment" = YearOfLastAssessment)
      
      datatable(detail_df,filter = "top",
                            options = list(pageLength = 20,
                                           dom = "tip",
                                           lengthMenu = c(5, 10, 15, 20)),
                                rownames = FALSE)
    })

    output$n_SID <- renderText({
      req(!is.null(checks_data()$SID))
      nrow(checks_data()$SID)
    })
    
    output$n_SAG <- renderText({
      req(!is.null(checks_data()$SAG))
      nrow(checks_data()$SAG)
    })
    
    output$n_ASD <- renderText({
      req(!is.null(checks_data()$ASD))
      nrow(checks_data()$ASD)
    })
    
  })
}

## To be copied in the UI
# mod_db_checks_ui("db_checks_1")

## To be copied in the server
# mod_db_checks_server("db_checks_1")
