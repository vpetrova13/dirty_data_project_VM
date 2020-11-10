# Load libraries and read data
library(tidyverse)
library(janitor)
library(readxl)

candy_1 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx") %>% clean_names()
candy_2 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx") %>% clean_names()
candy_3 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx") %>% clean_names()  

#Candy 1 data
## Change data type to long one.
candy_1 <- candy_1 %>% 
  pivot_longer(4:124, names_to = "candies", values_to = "comments")
## Rename column names
candy_1 <- candy_1 %>% 
  rename(year = timestamp,
         age = how_old_are_you,
         going_or_not = are_you_going_actually_going_trick_or_treating_yourself)
## Change rownames
candy_1 <- candy_1 %>% 
  mutate(year = str_extract_all(year, "2015")) %>% 
  mutate(going_or_not = str_to_lower(going_or_not)) %>% 
  mutate(comments = str_to_lower(comments))

# Change age to numeric

candy_1 <- candy_1 %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year >= 0 & year <= 100)


unique(candy_1$age)
  
#Candy 2 data
## Change data type to long one.
candy_2 <- candy_2 %>% 
  pivot_longer(7:123, names_to = "candies", values_to = "comments")
## Rename column names
candy_2 <- candy_2 %>% 
  rename(year = timestamp,
         age = how_old_are_you,
         going_or_not = are_you_going_actually_going_trick_or_treating_yourself,
         country = which_country_do_you_live_in,
         province = which_state_province_county_do_you_live_in,
         gender = your_gender)
## Change rownames
candy_2 <- candy_2 %>% 
  mutate(year = str_extract_all(year, "2016")) %>% 
  mutate(going_or_not = str_to_lower(going_or_not)) %>% 
  mutate(comments = str_to_lower(comments)) %>% 
  mutate(country = str_to_lower(country)) %>% 
  mutate(province = str_to_lower(province)) %>% 
  mutate(gender = str_to_lower(gender))


## Recode observations in data
# Change US
candy_2 <- candy_2 %>% 
  mutate(country = recode(country,
    "united state" = "usa",
    "united states of america" = "usa",
    "usa usa usa" = "usa",
    "the best one - usa" = "usa",
    "usa! usa! usa!" = "usa",
    "units states" = "usa",
    "us" = "usa",
    "united states" = "usa",
    "ussa" = "usa",
    "u.s.a." = "usa",
    "usa!" = "usa",
    "usa (i think but it's an election year so who can really tell)" = "usa",
    "u.s." = "usa",
    "america" = "usa",
    "the yoo ess of aaayyyyyy" = "usa",
    "united sates" = "usa",
    "usa usa usa usa" = "usa",
    "united  states of america" = "usa",
    "usa!!!!!!" = "usa",
    "usa! usa!" = "usa",
    "united stetes" = "usa",
    "sub-canadian north america... 'merica" = "usa",
    "trumpistan" = "usa",
    "merica" = "usa",
    "murica" = "usa"
  ))

##EU and others
candy_2 <- candy_2 %>% 
  mutate(country = recode(country, 
                          "england" = "uk",
                          "united kingdom" = "uk",
                          "a tropical island south of the equator" = "unknown",
                          "neverland" = "unknown",
                          "this one" = "unknown",
                          "korea" = "south korea",
                          "denial"= "unknown",
                          "not the usa or canada" = "unknown",
                          "see above" = "unknown",
                          "eua" = "unknown",
                          "god's country" = "unknown",
                          "somewhere"  = "unknown",
                          "one of the best ones"  = "unknown",
                          "there isn't one for old men"= "unknown",
                          "51.0" = "unknown",
                          "47.0"  = "unknown",
                          "54.0"= "unknown",
                          "44.0" = "unknown",
                          "45.0" = "unknown",
                          "30.0" = "unknown",
                          "the netherlands" = "netherland",
                          "cascadia" = "unknown",
                          "the republic of cascadia"= "unknown",
                          "netherland" = "netherlands",
                          "españa" = "spain",
                          "united kindom" = "uk"
                          ))


