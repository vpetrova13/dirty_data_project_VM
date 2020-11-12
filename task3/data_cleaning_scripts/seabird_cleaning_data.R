#Load libraries 
library(tidyverse)
library(janitor)
library(readxl)

#Explore raw data and sheets
excel_sheets(path = "raw_data/seabirds.xls")
length(excel_sheets(path = "raw_data/seabirds.xls"))

#Read data
seabird_raw_ship <- read_xls(path = "raw_data/seabirds.xls", 
                        sheet = "Ship data by record ID") %>% clean_names()
seabird_raw_bird <- read_xls(path = "raw_data/seabirds.xls", 
                        sheet = "Bird data by record ID") %>% clean_names()

#Remove unneeded variables (columns) by analysis another sheets from excel file
names(seabird_raw_bird)
bird_codes <- read_xls(path = "raw_data/seabirds.xls", 
                             sheet = "Bird data codes") %>% clean_names()
ship_codes <- read_xls(path = "raw_data/seabirds.xls", 
                       sheet = "Ship data codes") %>% clean_names()

seabird_data <- seabird_raw_bird %>% 
  select(record:age, sex, count)

ship_data <- seabird_raw_ship %>% 
  select(record:long, obs, csmeth)

# Join data
seabird_full_data <- left_join(seabird_data, ship_data, by = "record_id")

#Remove more columns which are not needed for further analysis
seabird_full_data <- seabird_full_data %>% 
  select(-sex, -date, -time, -obs, -csmeth, -age)

#Write csv file

write_csv(seabird_full_data, "seabird_clean_data.csv")





