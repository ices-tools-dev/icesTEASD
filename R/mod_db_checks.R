#' db_checks UI Function
#'
#' @description A shiny Module to identify errors in, and mismatches between, ICES SID and SAG databases
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#'
#' @import dplyr
#' @importFrom shiny NS tagList renderUI reactive bindEvent req renderText selectInput
#' @importFrom DT renderDT renderDataTable datatable
#' @importFrom lubridate year month
#' @importFrom shinycssloaders withSpinner
#' @importFrom bslib layout_sidebar sidebar
#' @importFrom lubridate year
mod_db_checks_ui <- function(id){
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(open = T, bg = "white", fg = "black", 
        uiOutput(outputId = ns("year_selector")),
        uiOutput(ns("advice_preview")),
        actionButton(inputId = ns("check"), label = "Check for mismatches",
                     class = "btn btn-primary")
      ),
      tabsetPanel(
        tabPanel("Overview",
                 card(
                   card_header("Issues"),
                     layout_column_wrap(
                       fill = T,
                       width = 1/3,  
                       value_box(
                         title = "SID",
                         value = textOutput(ns("n_SID"))),
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
    
    output$advice_preview <- renderUI({
      req(input$year)
        if(input$year ==year(Sys.Date())){
          numericInput(ns("preview_days"), "Preview upcoming issues:\nADG end-date - n (days)", min = 0, max = 365, value = 7)
        }
    })

    selected_year <- reactive({
      req(input$check)
      as.numeric(input$year)
    })
    
    
    stock_db_data <- reactive({
    
      data <- get_stock_data(as.numeric(input$year))
      
    })
    
    prepped_data <- reactive({
      req(!is.null(stock_db_data()))
      
      data <- prepare_stock_data(stock_db_data()$SID_data,
                                 stock_db_data()$SAG_data_raw,
                                 stock_db_data()$ASD_data,
                                 advice_validity_year = selected_year())
    })

    
    checks_data <- reactive({
      req(!is.null(prepped_data()))
      issues <-check_stock_db_errors(SID_data = prepped_data()$SID_data, 
                                     SID_selected_year = prepped_data()$SID_selected_year,
                                     SAG_advice = prepped_data()$SAG_advice,
                                     SAG_not_advice = prepped_data()$SAG_not_advice,
                                     ASD_valid_advice = prepped_data()$ASD_valid_advice,
                                     SAG_ASD_matched = prepped_data()$SAG_ASD_matched,
                                     advice_validity_year = selected_year())
    }) %>% bindEvent(input$check)
    
    
    checks_data_filtered <- reactive({
      req(!is.null(checks_data()))
      dat <- list(SID = checks_data()$SID,
                  SAG = checks_data()$SAG,
                  ASD = checks_data()$ASD)
                  
        if(selected_year() == year(Sys.Date())){
          dat$SID <- dat$SID %>% 
            filter(!(advice_release_date-as.numeric(input$preview_days)) > Sys.Date() | is.na(advice_release_date))
        }
      
      dat$issue_count <- map_df(dat, ~ select(.x, ExpertGroup)) %>% 
        summarise(.by = ExpertGroup, Count = n()) %>% 
        arrange(desc(Count))
      
      return(dat)
    })

    
    ######### Outputs #########
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
  })
}