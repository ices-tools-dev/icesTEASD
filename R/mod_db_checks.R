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
            )),
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
      selectInput(inputId = ns("year"), label = "Select Assessment Year", choices = years, selected = default_year, multiple = F, width = "100%")
    })


    data <- reactive({
      
      issues <-check_stock_db_errors(year = as.numeric(input$year))
      
    }) %>%
      bindEvent(input$check)


    output$EG_table <- renderDT({
      req(!is.null(data()))
      
      datatable(data()$issue_count, options = list(pageLength = 20, 
                                      dom = "tip", 
                                      lengthMenu = c(5, 10, 15, 20)),
                    rownames = FALSE)
    })


    output$SID <- renderDT({
       req(!is.null(data()$SID))
      detail_df <- select(data()$SID, Stock, Issue, "Expert Group" = ExpertGroup, "Year Of Last Assessment" = YearOfLastAssessment, "Year Of Next Assessment" = YearOfNextAssessment)
      
      datatable(detail_df,filter = "top",
                            options = list(pageLength = 20,
                                           dom = "tip",
                                           lengthMenu = c(5, 10, 15, 20)),
                                rownames = FALSE)
    })
    
    output$SAG <- renderDT({
       req(!is.null(data()$SAG))
      detail_df <- select(data()$SAG, Stock, Issue, "Expert Group" = ExpertGroup, "Year Of Last Assessment" = YearOfLastAssessment)
      
      datatable(detail_df,filter = "top",
                            options = list(pageLength = 20,
                                           dom = "tip",
                                           lengthMenu = c(5, 10, 15, 20)),
                                rownames = FALSE)
    })
    
    output$ASD <- renderDT({
       req(!is.null(data()$ASD))
      detail_df <- select(data()$ASD, Stock, Issue, "Expert Group" = ExpertGroup, "Year Of Last Assessment" = YearOfLastAssessment)
      
      datatable(detail_df,filter = "top",
                            options = list(pageLength = 20,
                                           dom = "tip",
                                           lengthMenu = c(5, 10, 15, 20)),
                                rownames = FALSE)
    })

    output$n_SID <- renderText({
      req(!is.null(data()$SID))
      nrow(data()$SID)
    })
    
    output$n_SAG <- renderText({
      req(!is.null(data()$SAG))
      nrow(data()$SAG)
    })
    
    output$n_ASD <- renderText({
      req(!is.null(data()$ASD))
      nrow(data()$ASD)
    })
    
  })
}

## To be copied in the UI
# mod_db_checks_ui("db_checks_1")

## To be copied in the server
# mod_db_checks_server("db_checks_1")
