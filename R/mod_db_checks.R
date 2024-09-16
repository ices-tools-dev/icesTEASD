#' db_checks UI Function
#'
#' @description A shiny Module to identify errors in, and mismatches between, ICES SID and SAG databases
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinycssloaders withSpinner
#' @importFrom bslib layout_sidebar sidebar
mod_db_checks_ui <- function(id){
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(open = T, bg = "white", fg = "black", 
        uiOutput(outputId = ns("year_selector")),
        actionButton(inputId = ns("check"), label = "Check for mismatches",
                     class = "btn btn-primary"),
        numericInput(ns("preview_days"), "Advice release 'preview' (days)", min = 0, max = 365, value = 7)
      ),
      tabsetPanel(
        tabPanel("Overview",
                 card(
                   card_header(bs_icon("wrench")),
                     layout_column_wrap(
                       fill = T,
                       width = 1/3,  
                       value_box(
                         title = "SID",
                         value = textOutput(ns("n_SID")),),
                       value_box(
                         title = "SAG",
                         value = textOutput(ns("n_SAG")),
                         showcase = NULL),
                       value_box(
                         title = "ASD",
                         value = textOutput(ns("n_ASD")),
                         showcase = NULL),
                     )
                   ),
                   card(card_header("Overview"),
                        withSpinner(dataTableOutput(outputId = ns("EG_table")), type = 8), full_screen = T)
        ),
        tabPanel("SID",
                 card(card_header("SID"),
                      withSpinner(DTOutput(outputId = ns("SID")), type = 8), full_screen = T)
                 ),
        tabPanel("SAG",
                 card(card_header("SAG"),
                      withSpinner(DTOutput(outputId = ns("SAG")), type = 8), full_screen = T)),
        tabPanel("ASD",
                 card(card_header("ASD"),
                      withSpinner(DTOutput(outputId = ns("ASD")), type = 8), full_screen = T))
        
      )
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

    selected_year <- reactive({
      req(input$check)
      as.numeric(input$year)
    }) %>% bindEvent(input$check) 
    
    
    stock_data <- reactive({
    
      data <- get_stock_data(as.numeric(input$year))
      
    }) %>% bindEvent(input$check)

    
    checks_data <- reactive({
      req(!is.null(stock_data()))
      issues <-check_stock_db_errors(stock_data()$SID_data,
                                     stock_data()$SAG_data_raw,
                                     stock_data()$ASD_data,
                                     year = selected_year())
    }) %>% bindEvent(input$check)
    
    
    checks_data_filtered <- reactive({
      dat <- list(SID = checks_data()$SID,
                  SAG = checks_data()$SAG,
                  ASD = checks_data()$ASD)
                  
        if(selected_year() == lubridate::year(Sys.Date())){
          dat$SID <- dat$SID %>% 
            filter(!(advice_release_date-as.numeric(input$preview_days)) > Sys.Date() | is.na(advice_release_date))
        }
      
      dat$issue_count <- map_df(dat, ~ select(.x, ExpertGroup)) %>% 
        summarise(.by = ExpertGroup, Count = n()) %>% 
        arrange(desc(Count))
      
      return(dat)
    })

    
    output$EG_table <- renderDT({
      req(!is.null(checks_data_filtered()))
      
      datatable(checks_data_filtered()$issue_count, options = list(pageLength = 20, 
                                      dom = "tip", 
                                      lengthMenu = c(5, 10, 15, 20)),
                    rownames = FALSE)
    })


    output$SID <- renderDT({
       req(!is.null(checks_data_filtered()$SID))
      detail_df <- select(checks_data_filtered()$SID, Stock, Issue, "Expert Group" = ExpertGroup, "Year Of Last Assessment" = YearOfLastAssessment, "Year Of Next Assessment" = YearOfNextAssessment)
      
      datatable(detail_df,filter = "top",
                            options = list(pageLength = 20,
                                           dom = "tip",
                                           lengthMenu = c(5, 10, 15, 20)),
                                rownames = FALSE)
    })

        
    output$SAG <- renderDT({
       req(!is.null(checks_data_filtered()$SAG))

      detail_df <- select(checks_data_filtered()$SAG, Stock, 
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
       req(!is.null(checks_data_filtered()$ASD))

      detail_df <- select(checks_data_filtered()$ASD, 
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
      req(!is.null(checks_data_filtered()$SID))
      nrow(checks_data_filtered()$SID)
    })
    
    output$n_SAG <- renderText({
      req(!is.null(checks_data_filtered()$SAG))
      nrow(checks_data_filtered()$SAG)
    })
    
    output$n_ASD <- renderText({
      req(!is.null(checks_data_filtered()$ASD))
      nrow(checks_data_filtered()$ASD)
    })
    
  })
}

## To be copied in the UI
# mod_db_checks_ui("db_checks_1")

## To be copied in the server
# mod_db_checks_server("db_checks_1")
