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
#' @importFrom dplyr filter select mutate bind_rows left_join summarise n arrange
#' @importFrom magrittr %>%
#' @importFrom shiny validate need
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_df
#'
check_stock_db_errors <- function(year) {

  url <- paste0(
    "http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear%20eq%20",
    year
  )
  out <- fromJSON(url, simplifyDataFrame = TRUE)$value
  SID_data <- unique(out)

    
  SAG_data <- getSAG_complete(year = year) 
  names(SAG_data)[names(SAG_data) == "FishStock"] <- "StockKeyLabel"
 
   
  years <- seq(year, year-3)
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
  
  ASD_valid_advice_data <- dplyr::filter(ASD_data, assessmentKey %in% SAG_advice_data$AssessmentKey)
  
  SID_errors <-
    SID_data %>%
    filter(YearOfNextAssessment <= year) %>%
    select(Stock = StockKeyLabel) %>%
    mutate(Database = "SID",
           Issue = "Please check Year of Next Assessment")


  mismatch_missing_in_SID <-
    data.frame(Stock = setdiff(SAG_advice_data$StockKeyLabel, SID_data$StockKeyLabel)) %>%
    mutate(Database = "SID",
      Issue = "Missing entry for the selected year"
    )
  

  mismatch_missing_in_SAG <-
    data.frame(Stock = setdiff(SID_selected_year$StockKeyLabel, SAG_advice_data$StockKeyLabel)) %>%
    mutate(Database = "SAG", 
           Issue = "Missing entry for relevant assessment year")
  

  mismatch_missing_in_SAG[mismatch_missing_in_SAG$Stock %in% SAG_data$StockKeyLabel,] <- "No SAG entry with Purpose == Advice"
  
  
  mismatches_SAG_ASD <-
      data.frame(Stock = setdiff(SAG_data$StockKeyLabel, ASD_valid_advice_data$stockCode)) %>%
      mutate(Database = "ASD",
             Issue = "Missing entry for relevant assessment year")

  
  missing_ASD <- data.frame(Stock = mismatch_missing_in_SAG[mismatch_missing_in_SAG$Issue == "Missing entry for relevant assessment year", "Stock"]) %>% 
    mutate(Database = "ASD",
           Issue = "Missing entry for relevant assessment year")
  
  
  replaced_advice <-
      data.frame(Stock = setdiff(ASD_data[ASD_data$adviceStatus == "Replaced", ]$stockCode, ASD_data[ASD_data$adviceStatus == "Advice", ]$stockCode)) %>%
      mutate(Database = "ASD",
             Issue = "Replaced advice; latest advice missing")
  

  selected_SAG_data <- select(SAG_data, AssessmentKey, "Assessment Year" = AssessmentYear, StockKeyLabel)
  
  SID <- bind_rows(SID_errors, mismatch_missing_in_SID) %>% 
    join_expert_group(SID_data = SID_data, match_column = "Stock", year = year) %>% 
    arrange(Stock)
  
  SAG <- mismatch_missing_in_SAG %>% 
    join_expert_group(SID_data = SID_data, match_column = "Stock", year = year) %>% 
    left_join(selected_SAG_data, by = c("Stock" = "StockKeyLabel")) %>% 
    arrange(Stock)
  
  ASD <-  bind_rows(mismatches_SAG_ASD, replaced_advice, missing_ASD) %>% 
    join_expert_group(SID_data = SID_data, match_column = "Stock", year = year) %>% 
    left_join(selected_SAG_data, by = c("Stock" = "StockKeyLabel")) %>% 
    arrange(Stock)
  
  eg <- purrr::map_df(list(SID, SAG, ASD), ~ select(.x, ExpertGroup)) %>% 
    summarise(.by = ExpertGroup, Count = n()) %>% 
    arrange(desc(Count))
  
  issues <- list(SID = SID,
                 SAG = SAG,
                 ASD = ASD,
                 issue_count = eg)
  

  return(issues)
}


