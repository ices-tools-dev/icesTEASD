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

#' Prepare SAG records that are not advice
#'
#' @description
#' Keeps the latest SAG record per stock and returns records where `Purpose` is
#' not `"Advice"`. Used to identify replaced or otherwise non-advice SAG entries.
#'
#' @param SAG_data_raw A data frame of raw SAG records.
#'
#' @return A data frame of latest non-advice SAG records with `FishStock` renamed
#' to `StockKeyLabel`.
#'
#' @noRd
prepare_SAG_not_advice <- function(SAG_data_raw){
  get_latest_SAG(SAG_data_raw) %>%
    rename("StockKeyLabel" = "FishStock") %>% 
    filter(Purpose != "Advice")
}

#' Prepare SAG advice records
#'
#' @description
#' Filters raw SAG records to advice entries, keeps the latest record per stock,
#' and standardises the stock column name.
#'
#' @param SAG_data_raw A data frame of raw SAG records.
#'
#' @return A data frame of latest SAG advice records with `FishStock` renamed to
#' `StockKeyLabel`.
#'
#' @noRd
prepare_SAG_advice <- function(SAG_data_raw){
  SAG_data_raw %>% filter(Purpose == "Advice") %>% 
    get_latest_SAG() %>%
    rename("StockKeyLabel" = "FishStock")
}

#' Prepare SID records for the selected advice validity year
#'
#' @description
#' Filters SID records to stocks where `YearOfLastAssessment` matches the
#' selected advice validity year.
#'
#' @param SID_data A data frame of SID stock records.
#' @param advice_validity_year The advice validity year to filter by.
#'
#' @return A filtered SID data frame.
#'
#' @noRd
prepare_SID_selected_year <- function(SID_data, advice_validity_year){
  SID_data %>%
    filter(YearOfLastAssessment == advice_validity_year)
}

#' Prepare ASD records with valid published advice
#'
#' @description
#' Filters ASD records to entries linked to SAG advice assessments and with
#' `adviceStatus == "Advice"`.
#'
#' @param ASD_data A data frame of ASD advice view records.
#' @param SAG_advice A data frame of prepared SAG advice records.
#'
#' @return A filtered ASD data frame containing valid published advice records.
#'
#' @noRd
prepare_ASD_valid_advice <- function(ASD_data, SAG_advice){
  filter(ASD_data, assessmentKey %in% SAG_advice$AssessmentKey, adviceStatus == "Advice")
}

#' Match SAG advice records to ASD records
#'
#' @description
#' Reduces SAG and ASD records to matching fields and performs a full join by
#' stock, assessment year, and assessment key.
#'
#' @param SAG_advice A data frame of prepared SAG advice records.
#' @param ASD_data A data frame of ASD advice view records.
#'
#' @return A data frame joining SAG advice records with corresponding ASD records.
#'
#' @noRd
prepare_SAG_ASD_matched <- function(SAG_advice, ASD_data){
  SAG_reduced <- select(SAG_advice, StockKeyLabel, AssessmentKey, Purpose, AssessmentYear)
  ASD_reduced <- select(ASD_data, adviceViewPublished, stockCode, adviceStatus, assessmentKey, assessmentYear)
  full_join(SAG_reduced,
            ASD_reduced,
            by = c("StockKeyLabel"="stockCode", "AssessmentYear"="assessmentYear", "AssessmentKey"="assessmentKey"))
}

#' Prepare stock data for database checks
#'
#' @description
#' Creates the derived SID, SAG, and ASD datasets needed by the stock database
#' validation checks.
#'
#' @param SID_data A data frame of SID stock records.
#' @param SAG_data_raw A data frame of raw SAG records.
#' @param ASD_data A data frame of ASD advice view records.
#' @param advice_validity_year The advice validity year used to select relevant
#' SID records.
#'
#' @return A named list containing prepared SID, SAG, and ASD data frames.
#'
#' @noRd
prepare_stock_data <- function(SID_data, SAG_data_raw, ASD_data, advice_validity_year){
  
  SAG_not_advice <- prepare_SAG_not_advice(SAG_data_raw)
  SAG_advice <- prepare_SAG_advice(SAG_data_raw)
  SID_selected_year <- prepare_SID_selected_year(SID_data, advice_validity_year = advice_validity_year)
  ASD_valid_advice <- prepare_ASD_valid_advice(ASD_data, SAG_advice)
  SAG_ASD_matched <- prepare_SAG_ASD_matched(SAG_advice, ASD_data)
  
  return(list(SID_data = SID_data,
              SID_selected_year = SID_selected_year,
              SAG_advice = SAG_advice,
              SAG_not_advice = SAG_not_advice,
              ASD_valid_advice = ASD_valid_advice,
              SAG_ASD_matched = SAG_ASD_matched))
  
}

#' Create an empty issue data frame
#'
#' @description
#' Creates an empty issue table with the standard columns used by the check
#' functions.
#'
#' @return An empty data frame with columns `Stock`, `Database`, and `Issue`.
#'
#' @noRd
empty_issue_df <- function() {
  data.frame(
    Stock = character(),
    Database = character(),
    Issue = character()
  )
}

#' Check for replaced SAG advice without a valid alternative
#'
#' @description
#' Identifies stocks where the latest non-advice SAG record appears to replace
#' advice, but no valid SAG advice record exists for the same stock.
#'
#' @param SAG_advice A data frame of prepared SAG advice records.
#' @param SAG_not_advice A data frame of prepared SAG records where `Purpose` is
#' not `"Advice"`.
#'
#' @return A data frame of SAG issues.
#'
#' @noRd
check_SAG_advice_replaced <- function(SAG_advice, SAG_not_advice){
 advice_replaced_stocks <- setdiff(SAG_not_advice$StockKeyLabel, SAG_advice$StockKeyLabel)
 SAG_not_advice %>% filter(StockKeyLabel %in% advice_replaced_stocks) %>% 
   mutate(Stock = StockKeyLabel,
          Database = "SAG",
          Issue = glue("SAG entry {AssessmentKey} has status 'Replaced' with no valid alternative in {AssessmentYear}"),
          .keep = "none"
   ) 
 
}

#' Check SID records with next assessment year in the past
#'
#' @description
#' Identifies SID records where `YearOfNextAssessment` is less than or equal to
#' the selected advice validity year.
#'
#' @param SID_data A data frame of SID stock records.
#' @param advice_validity_year The advice validity year to check against.
#'
#' @return A data frame of SID issues.
#'
#' @noRd
check_SID_next_assessment_in_past <- function(SID_data, advice_validity_year) {
  SID_data %>%
    filter(YearOfNextAssessment <= advice_validity_year) %>%
    select(Stock = StockKeyLabel) %>%
    mutate(Database = "SID",
           Issue = "Please check Years of Last/Next Assessment")
}


#' Check current-year SID ADG values
#'
#' @description
#' For the current calendar year only, identifies SID records where the next
#' assessment year is due and the `AdviceDraftingGroup` is not found in the
#' advice release table.
#'
#' @param SID_data A data frame of SID stock records.
#' @param advice_validity_year The advice validity year being checked.
#' @param advice_releases A data frame of advice releases containing an `ADG`
#' column.
#'
#' @return A data frame of SID issues, or an empty issue data frame when the
#' selected advice validity year is not the current year.
#'
#' @noRd
#'
#' @importFrom lubridate year
check_SID_current_year_ADG_invalid <- function(SID_data, advice_validity_year, advice_releases){
  
  current_year <- year(Sys.Date())
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

# Need to combine with check below or remove
#' Check required SID fields
#'
#' @description
#' Identifies SID records for the selected year with missing values in required
#' descriptive, assessment, advice, or guild fields.
#'
#' @param SID_selected_year A data frame of SID records selected for the advice
#' validity year.
#'
#' @return A data frame of SID issues describing which fields are empty.
#'
#' @noRd
check_SID_fields_complete_valid <- function(SID_selected_year){
  
  missing_fields <- SID_selected_year %>% 
    select(
      ActiveYear, StockKeyLabel, StockKeyDescription, DataCategory, SpeciesCommonName, SpeciesScientificName, 
      EcoRegion, ExpertGroup, AdviceDraftingGroup, YearOfLastAssessment, YearOfNextAssessment, 
      AssessmentFrequency, AssessmentType, AdviceType, UseOfDiscardsInAdvice,
      FisheriesGuild, SizeGuild, TrophicGuild
    ) %>% 
    filter(!complete.cases(.)) %>% 
    rowwise() %>% 
    mutate(
      missing_columns = paste0("Empty fields: ", paste(
        names(across(everything()))[sapply(across(everything()), is.na)],
        collapse = ", "
      ))
    ) %>% 
    ungroup() %>% 
    select(Stock = StockKeyLabel, Issue = missing_columns) %>% 
    mutate(Database = "SID")
  
}

#' Check SID guild information
#'
#' @description
#' Identifies SID records with missing guild information or guild fields that
#' contain typed `"NA"` values.
#'
#' @param SID_selected_year A data frame of SID records selected for the advice
#' validity year.
#'
#' @return A data frame of SID issues for missing or invalid guild information.
#'
#' @noRd
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

#' Check for SAG advice records missing from SID
#'
#' @description
#' Identifies stocks that have a SAG advice record but no corresponding SID
#' record.
#'
#' @param SID_data A data frame of SID stock records.
#' @param SAG_advice A data frame of prepared SAG advice records.
#'
#' @return A data frame of SID issues.
#'
#' @noRd
check_SID_missing_entry <- function(SID_data, SAG_advice) {
  
  data.frame(Stock = setdiff(SAG_advice$StockKeyLabel, SID_data$StockKeyLabel)) %>%
  mutate(Database = "SID",
    Issue = "Missing entry for the selected year"
  )
}

# Should it only be SAG advice, or also including Replaced -- This would create hundreds of rows of "errors", but are they errors?
# But should it be SID selected year not SID_data? No, SID selected year uses YearOfLastAssessment
# And should the match be done with the AssessmentKey? Tested, no Impact
# therefore - correct/best as is!

#' Check SID last assessment year against SAG
#'
#' @description
#' Compares SID and SAG assessment years and identifies stocks where
#' `YearOfLastAssessment` in SID is earlier than the SAG `AssessmentYear`.
#'
#' @param SID_data A data frame of SID stock records.
#' @param SAG_advice A data frame of prepared SAG advice records.
#'
#' @return A data frame of SID issues for inconsistent assessment years.
#'
#' @noRd
check_SID_last_assessment <- function(SID_data, SAG_advice) {

  SAG_dat <- SAG_advice %>% select(StockKeyLabel, StockKey, AssessmentYear, AssessmentKey)
  SID_dat <- SID_data %>% select(StockKeyLabel, StockKey, YearOfLastAssessment, YearOfNextAssessment, AssessmentKey)
  SID_SAD_joined <- left_join(SAG_dat, SID_dat, by = "StockKeyLabel")
  SID_SAD_joined %>% filter(YearOfLastAssessment < AssessmentYear) %>% arrange(StockKeyLabel) %>% 
    select(StockKeyLabel) %>% 
    mutate(Stock = StockKeyLabel,
           Database = "SID",
           Issue = "Please check Years of Last/Next Assessment")%>% 
    select(Stock, Database, Issue)
}


# 1. Why does this use SID_selected_year whilst check_SID_missing_entry is SID_data
#     It probably shouldn't - my investigation with assessment year 2025 suggests SID_data correctly picks up more errors
# 2. Does it need to be split and a separate check for ASD be made?

#' Check for SID records missing from SAG
#'
#' @description
#' Identifies stocks selected in SID for the relevant assessment year that do not
#' have a corresponding SAG advice record.
#'
#' @param SID_selected_year A data frame of SID records selected for the advice
#' validity year.
#' @param SAG_advice A data frame of prepared SAG advice records.
#'
#' @return A data frame of SAG issues.
#'
#' @noRd
check_SAG_missing_entry <- function(SID_selected_year, SAG_advice) {
  data.frame(Stock = setdiff(SID_selected_year$StockKeyLabel, SAG_advice$StockKeyLabel)) %>%
    mutate(Database = "SAG", 
           Issue = "Missing entry in SAG and ASD for relevant assessment year")
}                                                     

#' Check ASD entries against SAG advice
#'
#' @description
#' Identifies SAG advice assessments that are missing from ASD or where the ASD
#' record does not have valid advice status.
#'
#' @param SAG_ASD_matched A data frame matching SAG advice records to ASD records.
#' @param ASD_valid_advice A data frame of valid ASD advice records.
#'
#' @return A data frame of ASD issues.
#'
#' @noRd
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


#' Run stock database consistency checks
#'
#' @description
#' Runs SID, SAG, and ASD consistency checks and returns issue tables grouped by
#' database.
#'
#' @param SID_data A data frame of SID stock records.
#' @param SID_selected_year A data frame of SID records selected for the advice
#' validity year.
#' @param SAG_advice A data frame of prepared SAG advice records.
#' @param SAG_not_advice A data frame of prepared SAG records where `Purpose` is
#' not `"Advice"`.
#' @param ASD_valid_advice A data frame of valid ASD advice records.
#' @param SAG_ASD_matched A data frame matching SAG advice records to ASD records.
#' @param advice_validity_year The advice validity year being checked.
#'
#' @return A named list with issue data frames for `SID`, `SAG`, and `ASD`.
#'
#' @noRd
check_stock_db_errors <- function(SID_data,
                                  SID_selected_year,
                                  SAG_advice,
                                  SAG_not_advice,
                                  ASD_valid_advice,
                                  SAG_ASD_matched,
                                  advice_validity_year){

  
  SID_last_assessment_invalid <- check_SID_last_assessment(SID_data, SAG_advice)
  SID_next_assessment_in_past <- check_SID_next_assessment_in_past(SID_data, advice_validity_year)
  SID_assessment_years_invalid <- rbind(SID_last_assessment_invalid, SID_next_assessment_in_past) %>% filter(!duplicated(.))
  SID_current_year_ADG_invalid <- check_SID_current_year_ADG_invalid(SID_data, advice_validity_year, advice_releases)
  SID_guild_detail_missing_invalid <- check_SID_guild_information_present_valid(SID_selected_year)
  SID_missing_entry <- check_SID_missing_entry(SID_data, SAG_advice)
  SID_errors <- bind_rows(SID_assessment_years_invalid, SID_current_year_ADG_invalid, SID_missing_entry, SID_guild_detail_missing_invalid) %>% 
    join_expert_group(SID_data = SID_data, match_column = "Stock") %>% 
    arrange(Stock) %>% 
    left_join(advice_releases, by = c("AdviceDraftingGroup" = "ADG"))
  
  selected_SAG_data <- select(SAG_advice, AssessmentKey, "Assessment Year" = AssessmentYear, StockKeyLabel)
  SAG_advice_replaced <- check_SAG_advice_replaced(SAG_advice, SAG_not_advice)
  
  ### Need to resolve!
  SAG_missing_entry <- check_SAG_missing_entry(SID_selected_year, SAG_advice)
  alt_SAG_missing_entry <- check_SAG_missing_entry(SID_data, SAG_advice)
  ###
  
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