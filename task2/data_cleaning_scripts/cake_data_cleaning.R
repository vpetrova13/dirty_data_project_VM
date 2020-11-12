# Load libraries
library(tidyverse)
library(janitor)
library(assertr)
# Read data
cake_data <- read_csv("raw_data/cake/cake-ingredients-1961.csv") 
# Explore raw data
head(cake_data)
dim(cake_data)
glimpse(cake_data)
# It can be seen that there are loads of NA and columns.

cake_names <- read_csv("raw_data/cake/cake_ingredient_code.csv")

#Change data into long format from wide format and combine two data.

cake_data_transpose <- cake_data %>% 
  pivot_longer(cols = 2:35, names_to = "Code", values_to = "value") %>% 
  pivot_wider("Code", names_from = "Cake", values_from =  "value")

comb_data <- inner_join(cake_names, cake_data_transpose, by = c("code" = "Code"))

comb_data <- clean_names(comb_data)

# Find how many NA in each column

comb_data %>% 
  summarise_all(funs(sum(is.na(.))))

# Replace NA in measure with one cup
comb_data <- comb_data %>% 
  mutate(ingredient = str_remove(string = ingredient, pattern = "cup"))

comb_data <- comb_data %>%
  mutate(measure = coalesce(replace_na(measure, "cup")))

comb_data <- comb_data %>%
  mutate(code = str_to_lower(code)) %>% 
  mutate(ingredient = str_to_lower(ingredient))
  

# Write a clean data csv file
write_csv(comb_data, "clean_cake_data.csv")



#Wide data format
wide_clean_data <- comb_data %>% 
  pivot_longer(cols = 4:21, names_to = "cakes",
               values_to = "value") %>% 
  pivot_wider("cakes", names_from = c("ingredient", "measure"),
              values_from =  "value") 

write_csv(wide_clean_data, "clean_wide_cake_data.csv")

#Data with lots of obs where multiple ingredients
multiple_ingr <- comb_data %>% 
  pivot_longer(cols = 4:21, names_to = "cakes",
               values_to = "value")

write_csv(multiple_ingr, "clean_long_cake_data.csv")

#Check reproducibility by writing assertive programming
multiple_ingr %>% 
  verify(is.na(measure) | str_detect(measure, "[a-z]+")) %>% 
  verify(is.na(cakes) | str_detect(cakes, "[a-z]+")) %>% 
  verify(is.na(ingredient) | str_detect(ingredient, "[a-z]+"))










