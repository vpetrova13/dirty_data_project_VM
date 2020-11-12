library(tidyverse)
library(readr)
library(janitor)

decathlon_raw <- read_rds("~/dirty_data_project/task1/raw_data/decathlon.rds") %>% 
  clean_names()

# Remove row names by creating one more column
decathlon_data <- decathlon_raw %>% 
  rownames_to_column

# Change column names and some rownames
decathlon_data <- decathlon_data %>% 
  rename("run_100m" = "x100m",
         "run_400m" = "x400m",
         "hurdle_110m" = "x110m_hurdle",
         "run_1500m" = "x1500m",
         "names" = "rowname")

decathlon_data <- decathlon_data %>% 
  mutate(competition = str_to_lower(competition))

#Write csv file

write_csv(decathlon_data, "decathlon_clean_data.csv")
  
