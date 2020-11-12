#Load libraries and read data
library(tidyverse)
library(janitor)

rwa_raw_data <- read_csv("~/dirty_data_project/task5/raw_data/rwa.csv") %>% clean_names()
rwa_codes <- read_csv("~/dirty_data_project/task5/raw_data/rwa_codebook.txt") %>% clean_names()

#Remove 'warm up' questions and other unnecessary columns
rwa_data <- rwa_raw_data %>% 
  select(q3:q22, e3:e22, gender, age, major, hand, familysize, education, urban) 
 
unique(rwa_codes$this_data_was_collected_through_an_interactive_on_line_version_of_the_right_wing_authoritarianism_scale_in_2015)

#Remove obs with unrealistic values
rwa_data <- rwa_data %>% 
  filter(age <= 100 & age >= 5) %>% 
  filter(familysize >= 1 & familysize <= 20) %>% 
  mutate(major = str_to_lower(major))

#Recode some variables
rwa_data <- rwa_data %>% 
  mutate(gender = case_when(gender == 1 ~ "male",
                            gender == 2 ~ "female",
                            gender == 3 ~ "other")) %>% 
  mutate(hand = case_when(hand == 1 ~ "right",
                          hand == 2 ~ "left",
                          hand == 3 ~ "both")) %>% 
  mutate(education = case_when(education == 1 ~ "less than high school",
                               education == 2 ~ "high school",
                               education == 3 ~ "university degree",
                               education == 4 ~ "graduate degree")) %>% 
  mutate(urban = case_when(urban == 1 ~ "rural", 
                           urban == 2 ~ "suburban",
                           urban == 3 ~ "urban"))

#Write clean csv data 
write_csv(rwa_data, "clean_rwa_data.csv")
