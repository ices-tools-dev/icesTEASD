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
        2023
    )
    out <- fromJSON(url, simplifyDataFrame = TRUE)$value
    sid_data <- unique(out)
    head(sid_data)
    year <- 2023
    # sid_data <- getSD(year = year) # - need to resove issue with libsodium
    sag_data <- icesSAG::StockList(year = year)

    validate(
        need(!is.null(sid_data), "SID not responding correctly"),
        need(!is.null(sag_data), "SAG not responding correctly")
    )
    sag_data <- sag_data %>% filter(Purpose == "Advice")
    head(sag_data)
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


    asd_data <- icesASD::getAdviceViewRecord(year = year)
    mismatches_SAG_ASD <-
        data.frame(Stock = setdiff(sag_data$StockKeyLabel, asd_data$stockCode)) %>%
        mutate(Issue = "Stock missing from ASD")
    # replaced <- asd_data[asd_data$adviceStatus == "Replaced", ]
    replaced_advice <-
        data.frame(Stock = setdiff(asd_data[asd_data$adviceStatus == "Replaced", ]$stockCode, asd_data[asd_data$adviceStatus == "Advice", ]$stockCode)) %>%
        mutate(Issue = "Replaced advice, missing from ASD")
    # mismatch_missing_in_ASD <- rbind(mismatch_missing_in_ASD, replaced_advice)

    return(
        bind_rows(
            SID_errors,
            mismatch_missing_in_SID,
            mismatch_missing_in_SAG,
            mismatches_SAG_ASD,
            replaced_advice
        )
    )
}