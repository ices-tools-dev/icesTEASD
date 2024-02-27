#' user_checks UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList passwordInput
mod_user_checks_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      card(
        passwordInput(inputId = ns("token"), label = "Paste token here:", value = "", width = "100%"),
        actionButton(
          inputId = ns("login"), label = "login with token",
          class = "btn btn-primary"
        ),
        layout_column_wrap(
          uiOutput(outputId = ns("year_selector")),
          uiOutput(outputId = ns("eg_selector"))
        ),
        actionButton(
          inputId = ns("check"), label = "Check for issues",
          class = "btn btn-primary"
        ),
        value_box(
          title = "Stock Database user issues",
          value = textOutput(ns("n_errors")),
          showcase = bs_icon("wrench")
        ),
        full_screen = TRUE
      ),
      card(
        card_header("User Info"),
        dataTableOutput(outputId = ns("user_table")),
        full_screen = TRUE
      )
    ),
    card(
      height = "600px",
      card_header("Detail"),
      p("Table of errors"),
      full_screen = TRUE
    )
  )
}

#' user_checks Server Functions
#'
#' @importFrom shiny moduleServer
#' @importFrom icesConnect ices_get_jwt decode_token
#' @importFrom httr content
#' @noRd
mod_user_checks_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # user login items
    user <- reactive({
      ices_user <- list(user = "invalid token supplied")
      if (nzchar(input$token)) {
        resp <- ices_get_jwt("https://taf.ices.dk/api/user", jwt = input$token)
        if (!is.null(resp) && httr::status_code(resp) == 200) {
          ices_user <- content(resp,
            simplifyVector = TRUE, type = "application/json"
          )
          ices_user$decoded <- decode_token(input$token)
        }
      }
      ices_user
    }) %>%
      bindEvent(input$login)

    output$user_table <- renderDT({
      is_staff <- "ICES staff" %in% user()$decoded$sharepoint
      user_df <- data.frame(claim = c("Name", "Email", "ICES Staff?"), value = c(user()$name, user()$email, is_staff))

      datatable(user_df,
        options = list(
          pageLength = 3,
          dom = "tip"
        ),
        rownames = FALSE
      )
    })

    # year and EG select items
    output$year_selector <- renderUI({
      years <- seq(year(Sys.Date()), year(Sys.Date()) - 7)
      if (month(Sys.Date()) <= 5) {
        default_year <- years[2]
      } else {
        default_year <- years[1]
      }
      selectInput(inputId = ns("year"), label = "Select Assessment Year", choices = years, selected = default_year, multiple = F, width = "100%")
    })

    output$eg_selector <- renderUI({
      years <- seq(year(Sys.Date()), year(Sys.Date()) - 7)
      if (month(Sys.Date()) <= 5) {
        default_year <- years[2]
      } else {
        default_year <- years[1]
      }
      selectInput(inputId = ns("year"), label = "Select Assessment Year", choices = years, selected = default_year, multiple = F, width = "100%")
    })

    data <- reactive({
      check_stock_db_errors(year = input$year)
    }) %>%
      bindEvent(input$check)


    output$EG_table <- renderDT({
      req(!is.null(data()))

      eg_df <- data() %>%
        summarise(.by = ExpertGroup, Issues = n()) %>%
        arrange(desc(Issues))

      datatable(eg_df,
        options = list(
          pageLength = 20,
          dom = "tip",
          lengthMenu = c(5, 10, 15, 20)
        ),
        rownames = FALSE
      )
    })


    output$detail_table <- renderDT({
      req(!is.null(data()))
      detail_df <- select(data(), c(Stock, Issue, ExpertGroup, YearOfLastAssessment, YearOfNextAssessment, AssessmentFrequency)) %>%
        arrange(Stock)

      datatable(detail_df,
        filter = "top",
        options = list(
          pageLength = 20,
          dom = "tip",
          lengthMenu = c(5, 10, 15, 20)
        ),
        rownames = FALSE
      )
    })


    output$n_errors <- renderText({
      req(!is.null(data()))
      nrow(data())
    })
  })
}

## To be copied in the UI
# mod_user_checks_ui("user_checks_1")

## To be copied in the server
# mod_user_checks_server("user_checks_1")
