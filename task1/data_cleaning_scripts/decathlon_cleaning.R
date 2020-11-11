library(tidyverse)
library(readr)
library(janitor)

decathlon_raw <- read_rds("~/dirty_data_project/task1/raw_data/decathlon.rds") %>% 
  clean_names()

decathlon_data <- decathlon_raw %>% 
  rownames_to_column
