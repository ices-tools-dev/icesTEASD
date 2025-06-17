## code to prepare `advice_releases` dataset goes here

library(readxl)
library(lubridate)
library(dplyr)

# path <- "data-raw/teasd_adg_advice_dates.xlsx"
# 
# advice_releases <- read_xlsx(path = path, na = "NA", col_types = c("date", "text", "text", "text")) %>% 
#   filter(!is.na(ADG)) %>% 
#   mutate(advice_release_date = lubridate::date(advice_release_date))
# 
# usethis::use_data(advice_releases, overwrite = TRUE)
# 
# 
# 

# raw_data <- read_xlsx("data-raw/data.xlsx")
# web_conf <- raw_data[grep("ACOM web" , raw_data$`Group Name`),c(1,2,7)]
# web_conf <- web_conf %>% mutate(ADG = gsub(pattern = "WC",replacement = "ADG", .data$`Meeting Name`))
# web_conf <- web_conf %>% mutate(ADG = gsub(pattern = " 2025",replacement = "", .data$`ADG`))
# names(web_conf)[3] <- "advice_release_date"
# writexl::write_xlsx(web_conf[,-2], col_names = T, path = "data-raw/web_conf_dates.xlsx")
# 
# advice_releases <- web_conf %>% 
#     filter(!is.na(ADG)) %>%
#     mutate(advice_release_date = lubridate::dmy(advice_release_date))


#Download the raw data on advice processes from PowerBI 
raw_data <- read_xlsx("data-raw/data.xlsx")
advice_releases <- raw_data %>% filter(str_detect(.data$`Meeting Name`, pattern = "ADG")) %>% 
  mutate(ADG = str_split_i(.data$`Meeting Name`, " ", i = 1),
         advice_release_date = lubridate::dmy(stringr::str_replace_all(.data$`Meeting End Date`, pattern = "/", replacement = "-"))) %>% 
  select(c(1,8,9))

usethis::use_data(advice_releases, overwrite = TRUE)

####

