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
#' @importFrom dplyr filter rename select mutate bind_rows left_join full_join anti_join summarise n arrange across case_when if_any
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

    
  SAG_data_raw <- getSAG_complete(year = year) 
  
  
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
    need(!is.null(SAG_data_raw), "SAG not responding correctly"),
    need(!is.null(ASD_data), "ASD not responding correctly")
  )

  return(list(SID_data=SID_data,
              SAG_data_raw=SAG_data_raw,
              ASD_data=ASD_data))
}


prepare_SAG_not_advice <- function(SAG_data_raw){
  get_latest_SAG(SAG_data_raw) %>%
    rename("StockKeyLabel" = "FishStock") %>% 
    filter(Purpose != "Advice")
}

prepare_SAG_advice <- function(SAG_data_raw){
  SAG_data_raw %>% filter(Purpose == "Advice") %>% 
    get_latest_SAG() %>%
    rename("StockKeyLabel" = "FishStock")
}

prepare_SID_selected_year <- function(SID_data, advice_validity_year){
  SID_data %>%
    filter(YearOfLastAssessment == advice_validity_year)
}

prepare_ASD_valid_advice <- function(ASD_data, SAG_advice){
  filter(ASD_data, assessmentKey %in% SAG_advice$AssessmentKey, adviceStatus == "Advice")
}

prepare_SAG_ASD_matched <- function(SAG_advice, ASD_data){
  SAG_reduced <- select(SAG_advice, StockKeyLabel, AssessmentKey, Purpose, AssessmentYear)
  ASD_reduced <- select(ASD_data, adviceViewPublished, stockCode, adviceStatus, assessmentKey, assessmentYear)
  full_join(SAG_reduced,
            ASD_reduced,
            by = c("StockKeyLabel"="stockCode", "AssessmentYear"="assessmentYear", "AssessmentKey"="assessmentKey"))
}

empty_issue_df <- function() {
  data.frame(
    Stock = character(),
    Database = character(),
    Issue = character()
  )
}

check_SAG_advice_replaced <- function(SAG_advice, SAG_not_advice){
 advice_replaced_stocks <- setdiff(SAG_not_advice$StockKeyLabel, SAG_advice$StockKeyLabel)
 SAG_not_advice %>% filter(StockKeyLabel %in% advice_replaced_stocks) %>% 
   mutate(Stock = StockKeyLabel,
          Database = "SAG",
          Issue = glue("SAG entry {AssessmentKey} has status 'Replaced' with no valid alternative in {AssessmentYear}"),
          .keep = "none"
   ) 
 
}

check_SID_next_assessment_in_past <- function(SID_data, advice_validity_year) {
  SID_data %>%
    filter(YearOfNextAssessment <= advice_validity_year) %>%
    select(Stock = StockKeyLabel) %>%
    mutate(Database = "SID",
           Issue = "Please check Year of Next Assessment")
}

check_SID_current_year_ADG_invalid <- function(SID_data, advice_validity_year, advice_releases){
  
  current_year <- lubridate::year(Sys.Date())
  if (advice_validity_year != current_year) {
    return(empty_issue_df())
  }
  
  SID_data %>%
      filter(YearOfNextAssessment <= advice_validity_year,
             !AdviceDraftingGroup %in% advice_releases$ADG) %>% 
      select(Stock = StockKeyLabel) %>%
      mutate(Database = "SID",
             Issue = "ADG info potentially incorrect")
}

check_SID_guild_information_present_valid <- function(SID_selected_year){
  #Could /should be SID_data as a whole?
  SID_selected_year %>% 
    select(StockKeyLabel, TrophicGuild, FisheriesGuild, SizeGuild) %>%
    mutate(Stock = StockKeyLabel,
           Database = "SID",
           Issue = case_when(if_any(c(TrophicGuild, FisheriesGuild, SizeGuild), ~ is.na(.)) ~ "Guild information missing",
                             if_any(c(TrophicGuild, FisheriesGuild, SizeGuild), ~ str_detect(., regex("^(na)$", ignore_case = TRUE))) ~ "Guild information contains typed NAs",
                             .default = NA)) %>% 
    filter(!is.na(Issue)) %>% 
    select(Stock, Database, Issue)
  
} 

check_SID_missing_entry <- function(SID_data, SAG_advice) {
  
  data.frame(Stock = setdiff(SAG_advice$StockKeyLabel, SID_data$StockKeyLabel)) %>%
  mutate(Database = "SID",
    Issue = "Missing entry for the selected year"
  )
}


# 1. Why does this use SID_selected_year whilst check_SID_missing_entry is SID_data
# 2. Does it need to be split and a separate check for ASD be made?
# 3. Following code removed - seemingly served no purpose: 
#      - SAG_missing_entry[SAG_missing_entry$Stock %in% SAG_advice$StockKeyLabel,] <- "No SAG entry with Purpose == Advice"
check_SAG_missing_entry <- function(SID_selected_year, SAG_advice) {
  data.frame(Stock = setdiff(SID_selected_year$StockKeyLabel, SAG_advice$StockKeyLabel)) %>%
    mutate(Database = "SAG", 
           Issue = "Missing entry in SAG and ASD for relevant assessment year")
}                                                     

check_ASD_missing_entry_vs_SAG <- function(SAG_ASD_matched, ASD_valid_advice){
  SAG_ASD_matched %>% filter(is.na(adviceStatus) | adviceStatus != "Advice") %>% 
    group_by(AssessmentYear, StockKeyLabel) %>% 
    anti_join(ASD_valid_advice, by = c("StockKeyLabel" = "stockCode", "AssessmentYear" = "assessmentYear")) %>%
    mutate(Database = "ASD",
           Issue = case_when(adviceStatus == "Replaced" ~ glue("ASD entry {AssessmentKey} has status 'Replaced' with no valid alternative in {AssessmentYear}"), 
                             is.na(adviceStatus) & Purpose == "Advice" ~ glue("No published entry in ASD for assessment {AssessmentKey} in {AssessmentYear} "))) %>%
    filter(is.na(adviceStatus) | adviceStatus != "Unofficial" | !is.na(Issue)) %>% 
    select(Stock = StockKeyLabel, Database, AssessmentKey, AssessmentYear, Issue)
  
}
check_stock_db_errors <- function(SID_data, SAG_data_raw, ASD_data, advice_validity_year){

  SAG_not_advice <- prepare_SAG_not_advice(SAG_data_raw)
  SAG_advice <- prepare_SAG_advice(SAG_data_raw)
  SID_selected_year <- prepare_SID_selected_year(SID_data, advice_validity_year = advice_validity_year)
  ASD_valid_advice <- prepare_ASD_valid_advice(ASD_data, SAG_advice)
  SAG_ASD_matched <- prepare_SAG_ASD_matched(SAG_advice, ASD_data)
  

  SID_next_assessment_in_past <- check_SID_next_assessment_in_past(SID_data, advice_validity_year)
  SID_current_year_ADG_invalid <- check_SID_current_year_ADG_invalid(SID_data, advice_validity_year, advice_releases)
  SID_guild_detail_missing_invalid <- check_SID_guild_information_present_valid(SID_selected_year)
  SID_missing_entry <- check_SID_missing_entry(SID_data, SAG_advice)
  SID_errors <- bind_rows(SID_next_assessment_in_past, SID_current_year_ADG_invalid, SID_missing_entry, SID_guild_detail_missing_invalid) %>% 
    join_expert_group(SID_data = SID_data, match_column = "Stock") %>% 
    arrange(Stock) %>% 
    left_join(advice_releases, by = c("AdviceDraftingGroup" = "ADG"))
  
  selected_SAG_data <- select(SAG_advice, AssessmentKey, "Assessment Year" = AssessmentYear, StockKeyLabel)
  SAG_advice_replaced <- check_SAG_advice_replaced(SAG_advice, SAG_not_advice)
  SAG_missing_entry <- check_SAG_missing_entry(SID_selected_year, SAG_advice)
  
  SAG_errors <- bind_rows(SAG_advice_replaced, SAG_missing_entry) %>% 
    join_expert_group(SID_data = SID_data, match_column = "Stock") %>% 
    left_join(selected_SAG_data, by = c("Stock" = "StockKeyLabel")) %>% 
    arrange(Stock)
  
  ASD_missing_entry_vs_SAG <- check_ASD_missing_entry_vs_SAG(SAG_ASD_matched, ASD_valid_advice)
  ASD_missing_entry <- data.frame(Stock = SAG_missing_entry[SAG_missing_entry$Issue == "Missing entry in SAG and ASD for relevant assessment year", "Stock"]) %>% 
    mutate(Database = "ASD",
           Issue = "Missing entry in SAG and ASD for relevant assessment year")
  ASD_errors <-  bind_rows(ASD_missing_entry_vs_SAG, ASD_missing_entry) %>% as.data.frame() %>% 
    join_expert_group(SID_data = SID_data, match_column = "Stock") %>% 
    filter(is.na(AssessmentYear) | AssessmentYear == YearOfLastAssessment | YearOfLastAssessment == 0) %>% 
    arrange(Stock)
  

  return(list(SID = SID_errors,
              SAG = SAG_errors,
              ASD = ASD_errors))

}