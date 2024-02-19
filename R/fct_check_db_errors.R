#' check_db_errors
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 
#' @importFrom icesFO load_sid
#' @importFrom icesSAG getListStocks
#' @importFrom dplyr filter select mutate
#'
check_stock_db_errors <- function(year) {
  
  sid_data <- load_sid(year)
  sag_data <- icesSAG::getListStocks(year) 
  
  validate(
    shiny::need(!is.null(sid_data), "SID not responding correctly"),
    shiny::need(!is.null(sag_data), "SAG not responding correctly")
  )
  sag_data <- sag_data %>% dplyr::filter(Purpose == "Advice")

  SID_errors <- sid_data %>% dplyr::filter(YearOfNextAssessment == year) %>%
    dplyr::select(Stock = StockKeyLabel) %>%
    dplyr::mutate(Issue = "Year of Next Assessment in past")

  sid_data <- sid_data %>% dplyr::filter(YearOfLastAssessment == year)

  mismatch_missing_in_SID <- data.frame(Stock = setdiff(sag_data$StockKeyLabel,sid_data$StockKeyLabel)) %>%
    dplyr::mutate(
                  Issue = "Stock missing from SID")
  mismatch_missing_in_SAG <- data.frame(Stock = setdiff(sid_data$StockKeyLabel,sag_data$StockKeyLabel)) %>%
    dplyr::mutate(Issue = "Stock missing from SAG")


  return(dplyr::bind_rows(SID_errors,
              mismatch_missing_in_SID,
              mismatch_missing_in_SAG))
}
