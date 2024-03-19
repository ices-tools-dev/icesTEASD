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
#' @importFrom dplyr filter select mutate bind_rows left_join
#' @importFrom magrittr %>%
#' @importFrom shiny validate need
#' @importFrom jsonlite fromJSON
#'
check_stock_db_errors <- function(year) {

  SAG_data <- getSAG_complete(year = year)
  
  names(SAG_data)[names(SAG_data) == "FishStock"] <- "StockKeyLabel"
  
  url <- paste0(
    "http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear%20eq%20",
    year
  )
  
  out <- fromJSON(url, simplifyDataFrame = TRUE)$value

  SID_data <- unique(out)
  
  # years <- seq(year, year-4)
  years <- seq(2023, 2019)
  
  ASD_data <- data.frame()
  for(i in years) {
    
  url <- paste0(
    "https://asd.ices.dk/api/getAdviceViewRecord?Year=",
    i
  )
  
  out <- fromJSON(url, simplifyDataFrame = TRUE)

  ASD_data <- dplyr::bind_rows(ASD_data, unique(out))
  }
  
  validate(
    need(!is.null(SID_data), "SID not responding correctly"),
    need(!is.null(SAG_data), "SAG not responding correctly"),
    need(!is.null(ASD_data), "ASD not responding correctly")
  )
  
  SAG_advice_data <- SAG_data %>% filter(Purpose == "Advice")
  SID_selected_year <- SID_data %>%
    filter(YearOfLastAssessment == year)
  ASD_valid_advice_data <- dplyr::filter(ASD_data, assessmentKey %in% SAG_data$AssessmentKey)
  
  SID_errors <-
    SID_data %>%
    filter(YearOfNextAssessment <= year) %>%
    select(Stock = StockKeyLabel) %>%
    mutate(Database = "SID",
           Issue = "Please check Year of Next Assessment")

  
  
  mismatch_missing_in_SID <-
    data.frame(Stock = setdiff(SAG_advice_data$StockKeyLabel, SID_data$StockKeyLabel)) %>%
    mutate(Database = "SID",
      Issue = "Missing entry in SID for the selected year"
    )
  
  mismatch_missing_in_SAG <-
    data.frame(Stock = setdiff(SID_selected_year$StockKeyLabel, SAG_advice_data$StockKeyLabel)) %>%
    mutate(Database = "SAG", 
           Issue = "Missing entry in SAG for the selected year")
  
  mismatch_missing_in_SAG[mismatch_missing_in_SAG$Stock %in% SAG_data$StockKeyLabel,] <- "No SAG entry with Purpose == Advice"
  
  mismatches_SAG_ASD <-
      data.frame(Stock = setdiff(SAG_data$StockKeyLabel, ASD_data$stockCode)) %>%
      mutate(Database = "ASD",
             Issue = "Missing entry in ASD for the selected year")
  
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


