## code to prepare `advice_releases` dataset goes here

library(readxl)
library(lubridate)
library(dplyr)

path <- "data-raw/teasd_adg_advice_dates.xlsx"
advice_releases <- read_xlsx(path = path, na = "NA", col_types = c("date", "text", "text", "text")) %>% 
  filter(!is.na(ADG)) %>% 
  mutate(advice_release_date = lubridate::date(advice_release_date))

usethis::use_data(advice_releases, overwrite = TRUE)
