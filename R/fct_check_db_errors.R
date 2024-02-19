#' check_db_errors
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @importFrom icesSAG getListStocks
#' @importFrom dplyr filter select mutate bind_rows
#' @importFrom magrittr %>%
#' @importFrom shiny validate need
#' @importFrom jsonlite fromJSON
#'
check_stock_db_errors <- function(year) {

  url <- paste0(
    "http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear%20eq%20",
    year
  )
  out <- fromJSON(url, simplifyDataFrame = TRUE)$value
  sid_data <- unique(out)

  # sid_data <- getSD(year = year) # - need to resove issue with libsodium
  sag_data <- getListStocks(year = year)

  validate(
    need(!is.null(sid_data), "SID not responding correctly"),
    need(!is.null(sag_data), "SAG not responding correctly")
  )
  sag_data <- sag_data %>% filter(Purpose == "Advice")

  SID_errors <-
    sid_data %>%
    filter(YearOfNextAssessment == year) %>%
    select(Stock = StockKeyLabel) %>%
    mutate(Issue = "Year of Next Assessment in past")


  sid_data <- sid_data %>% filter(YearOfLastAssessment == year)

  mismatch_missing_in_SID <-
    data.frame(Stock = setdiff(sag_data$StockKeyLabel, sid_data$StockKeyLabel)) %>%
    mutate(
      Issue = "Stock missing from SID"
    )

  mismatch_missing_in_SAG <-
    data.frame(Stock = setdiff(sid_data$StockKeyLabel, sag_data$StockKeyLabel)) %>%
    mutate(Issue = "Stock missing from SAG")

  return(
    bind_rows(
      SID_errors,
      mismatch_missing_in_SID,
      mismatch_missing_in_SAG
    )
  )
}
