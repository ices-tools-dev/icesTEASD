## code to prepare `advice_releases` dataset goes here

library(readxl)
library(lubridate)
library(dplyr)
library(stringr)

#Download the raw data on advice processes from PowerBI 

advice_releases <- read_xlsx("data-raw/ADGs2026.xlsx") %>% 
    mutate(ADG = str_split_i(.data$`Meeting Name`, " ", i = 1),
           advice_release_date = lubridate::ymd(stringr::str_replace_all(.data$`Meeting End Date`, pattern = "/", replacement = "-"))) %>%
  select(c("Request Topic", "ADG", "advice_release_date"))



usethis::use_data(advice_releases, overwrite = TRUE)

####

