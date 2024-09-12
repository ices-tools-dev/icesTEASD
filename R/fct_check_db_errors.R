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
#' @importFrom dplyr filter select mutate bind_rows left_join full_join anti_join summarise n arrange across case_when if_any
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom shiny validate need
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_df
#' @importFrom stringr str_detect regex
#'
get_stock_data <- function(year) {

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
  ASD_data <- bind_rows(ASD_data, unique(out))
  }
  
  validate(
    need(!is.null(SID_data), "SID not responding correctly"),
    need(!is.null(SAG_data), "SAG not responding correctly"),
    need(!is.null(ASD_data), "ASD not responding correctly")
  )
  return(list(SID_data=SID_data,
              SAG_data=SAG_data,
              ASD_data=ASD_data))
}

check_stock_db_errors <- function(SID_data, SAG_data, ASD_data, year, preview_days){
  
  
  SAG_advice_data <- SAG_data %>% filter(Purpose == "Advice")
  
  SID_selected_year <- SID_data %>%
    filter(YearOfLastAssessment == year)
  
  ASD_valid_advice_data <- filter(ASD_data, assessmentKey %in% SAG_advice_data$AssessmentKey, adviceStatus == "Advice")
  
  SID_errors <-
    SID_data %>%
    filter(YearOfNextAssessment <= year) %>%
    select(Stock = StockKeyLabel) %>%
    mutate(Database = "SID",
           Issue = "Please check Year of Next Assessment")


  detail_missing_in_SID <- SID_selected_year %>%
    select(StockKeyLabel, TrophicGuild, FisheriesGuild, SizeGuild) %>%
    mutate(Stock = StockKeyLabel,
           Database = "SID",
           Issue = case_when(if_any(c(TrophicGuild, FisheriesGuild, SizeGuild), ~ is.na(.)) ~ "Guild information missing",
                             if_any(c(TrophicGuild, FisheriesGuild, SizeGuild), ~ str_detect(., regex("^(na)$", ignore_case = TRUE))) ~ "Guild information contains typed NAs",
                             .default = NA)) %>% 
    filter(!is.na(Issue)) %>% 
    select(Stock, Database, Issue)

  mismatch_missing_in_SID <-
    data.frame(Stock = setdiff(SAG_advice_data$StockKeyLabel, SID_data$StockKeyLabel)) %>%
    mutate(Database = "SID",
      Issue = "Missing entry for the selected year"
    )
  

  mismatch_missing_in_SAG <-
    data.frame(Stock = setdiff(SID_selected_year$StockKeyLabel, SAG_advice_data$StockKeyLabel)) %>%
    mutate(Database = "SAG", 
           Issue = "Missing entry in SAG and ASD for relevant assessment year")
  

  mismatch_missing_in_SAG[mismatch_missing_in_SAG$Stock %in% SAG_data$StockKeyLabel,] <- "No SAG entry with Purpose == Advice"
  
  matched_SAG_ASD <- select(SAG_data, StockKeyLabel, AssessmentKey, Purpose, AssessmentYear) %>% full_join(select(ASD_data, adviceViewPublished, stockCode, adviceStatus, assessmentKey, assessmentYear), by = c("StockKeyLabel"="stockCode", "AssessmentYear"="assessmentYear", "AssessmentKey"="assessmentKey"))
  
  mismatches_SAG_ASD <- matched_SAG_ASD %>% filter(is.na(adviceStatus) | adviceStatus != "Advice") %>% 
    group_by(AssessmentYear, StockKeyLabel) %>% 
    anti_join(ASD_valid_advice_data, by = c("StockKeyLabel" = "stockCode", "AssessmentYear" = "assessmentYear")) %>%
    mutate(Database = "ASD",
           Issue = case_when(adviceStatus == "Replaced" ~ glue("ASD entry {AssessmentKey} has status 'Replaced' with no valid alternative in {AssessmentYear}"), 
                                    is.na(adviceStatus) & Purpose == "Advice" ~ glue("No published entry in ASD for assessment {AssessmentKey} in {AssessmentYear} "))) %>% 
    select(Stock = StockKeyLabel, Database, AssessmentKey, AssessmentYear, Issue)

  
  missing_ASD <- data.frame(Stock = mismatch_missing_in_SAG[mismatch_missing_in_SAG$Issue == "Missing entry in SAG and ASD for relevant assessment year", "Stock"]) %>% 
    mutate(Database = "ASD",
           Issue = "Missing entry in SAG and ASD for relevant assessment year")
  

  selected_SAG_data <- select(SAG_data, AssessmentKey, "Assessment Year" = AssessmentYear, StockKeyLabel)

  SID <- bind_rows(SID_errors, mismatch_missing_in_SID, detail_missing_in_SID) %>% 
    join_expert_group(SID_data = SID_data, match_column = "Stock") %>% 
    arrange(Stock) %>% 
    left_join(advice_releases, by = c("AdviceDraftingGroup" = "ADG"))
  
  if(year == lubridate::year(Sys.Date())){
    SID <- SID %>% 
      filter(!(advice_release_date-preview_days) > Sys.Date() | is.na(advice_release_date))
  }
  
  SAG <- mismatch_missing_in_SAG %>% 
    join_expert_group(SID_data = SID_data, match_column = "Stock") %>% 
    left_join(selected_SAG_data, by = c("Stock" = "StockKeyLabel")) %>% 
    arrange(Stock)

  ASD <-  bind_rows(mismatches_SAG_ASD, missing_ASD) %>% as.data.frame() %>% 
    join_expert_group(SID_data = SID_data, match_column = "Stock") %>% 
    filter(is.na(AssessmentYear) | AssessmentYear == YearOfLastAssessment | YearOfLastAssessment == 0) %>% 
    arrange(Stock)

  eg <- map_df(list(SID, SAG, ASD), ~ select(.x, ExpertGroup)) %>% 
    summarise(.by = ExpertGroup, Count = n()) %>% 
    arrange(desc(Count))
  
  issues <- list(SID = SID,
                 SAG = SAG,
                 ASD = ASD,
                 issue_count = eg)
  

  return(issues)

}


