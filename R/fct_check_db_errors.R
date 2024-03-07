#' check_db_errors
#'
#' @description A function running checks on SID and SAG database entries in a given year. 
#'
#' @return A \emph{dataframe} of stocks with identified issues
#' @param year A number
#'
#' @noRd
#'
#' @importFrom icesSAG getListStocks
#' @importFrom icesASD getAdviceViewRecord
#' @importFrom dplyr filter select mutate bind_rows left_join
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

  SID_data <- unique(out)
  SAG_data <- getListStocks(year = year)
  ASD_data <- getAdviceViewRecord(year = year)
  
  validate(
    need(!is.null(SID_data), "SID not responding correctly"),
    need(!is.null(SAG_data), "SAG not responding correctly"),
    need(!is.null(ASD_data), "ASD not responding correctly")
  )
  
  SAG_advice_data <- SAG_data %>% filter(Purpose == "Advice")
  SID_selected_year <- SID_data %>%
    filter(YearOfLastAssessment == year)
  # SID_data <- getSD(year = year) # - need to resove issue with libsodium

  SID_errors <-
    SID_data %>%
    filter(YearOfNextAssessment == year) %>%
    select(Stock = StockKeyLabel) %>%
    mutate(Database = "SID",
           Issue = "Year of Next Assessment in past")

  
  
  mismatch_missing_in_SID <-
    data.frame(Stock = setdiff(SAG_advice_data$StockKeyLabel, SID_selected_year$StockKeyLabel)) %>%
    mutate(Database = "SID",
      Issue = "Stock missing"
    )
  
  mismatch_missing_in_SAG <-
    data.frame(Stock = setdiff(SID_selected_year$StockKeyLabel, SAG_advice_data$StockKeyLabel)) %>%
    mutate(Database = "SAG", 
           Issue = "Stock missing")
  
  mismatch_missing_in_SAG[mismatch_missing_in_SAG$Stock %in% SAG_data$StockKeyLabel,] <- "No SAG entry with Purpose == Advice"
  
  mismatches_SAG_ASD <-
      data.frame(Stock = setdiff(SAG_data$StockKeyLabel, ASD_data$stockCode)) %>%
      mutate(Database = "ASD",
             Issue = "Stock missing")
  
  replaced_advice <-
      data.frame(Stock = setdiff(ASD_data[ASD_data$adviceStatus == "Replaced", ]$stockCode, ASD_data[ASD_data$adviceStatus == "Advice", ]$stockCode)) %>%
      mutate(Database = "ASD",
             Issue = "Replaced advice; latest advice missing")
 

  issues <- bind_rows(
      SID_errors,
      mismatch_missing_in_SID,
      mismatch_missing_in_SAG,
      mismatches_SAG_ASD,
    ) %>% join_expert_group(SID_data = SID_data, match_column = "Stock", year = year)

  return(issues)
}


