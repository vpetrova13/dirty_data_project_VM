# Load libraries and read data
library(tidyverse)
library(janitor)
library(readxl)
library(plyr)
library(assertr)

candy_1 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx") %>% clean_names()
candy_2 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx") %>% clean_names()
candy_3 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx") %>% clean_names()  

#Candy 1 data
## Change data type to long one.
candy_1 <- candy_1 %>% 
  pivot_longer(4:124, names_to = "candies", values_to = "comments")

## Rename column names
candy_1 <- candy_1 %>% 
  dplyr::rename(year = timestamp,
         age = how_old_are_you,
         going_or_not = are_you_going_actually_going_trick_or_treating_yourself) 

## Change rownames
candy_1 <- candy_1 %>% 
  mutate(year = str_extract_all(year, "2015") %>% as.numeric(year)) %>% 
  mutate(going_or_not = str_to_lower(going_or_not)) %>% 
  mutate(comments = str_to_lower(comments))

# Change age to numeric and filter strange values

unique(candy_1$age)

candy_1 <- candy_1 %>% 
  mutate(age = str_extract(age, "\\d+") %>% 
  as.numeric(age))

candy_1 <- candy_1 %>% 
  filter(age <= 100 & age >= 0)

  
#Candy 2 data
## Change data type to long one.
candy_2 <- candy_2 %>% 
  pivot_longer(7:123, names_to = "candies", values_to = "comments")
## Rename column names
candy_2 <- candy_2 %>% 
  dplyr::rename(year = timestamp,
         age = how_old_are_you,
         going_or_not = are_you_going_actually_going_trick_or_treating_yourself,
         country = which_country_do_you_live_in,
         province = which_state_province_county_do_you_live_in,
         gender = your_gender)
## Change rownames
candy_2 <- candy_2 %>% 
  mutate(year = str_extract_all(year, "2016") %>% 
           as.numeric(year)) %>% 
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

## Recode age column
candy_2 <- candy_2 %>% 
  mutate(age = recode(age, 
                      "Old enough to know better" = "unknown",
                      "old enough" = "unknown",
                      "As old as my tongue a few years older than my teeth"= "unknown",
                      "50s" = "50",
                      "old" = "unknown",
                      "0x2A" = "unknown",
                      "Fifty.  Nine.  Ish." = "59",
                      "Ancient" = "unknown",
                      "I remember the Nixon administration" = "unknown",
                      "over retirement age"  = "unknown",
                      "Old enough" = "unknown",
                      "50+" = "50",
                      "55+" = "55",
                      "over 40"  = "40",
                      "Hahahahahaha"= "unknown",
                      "1.0E18" = "unknown",
                      "Old"  = "unknown",
                      "Older than i act"  = "unknown",
                      "really old" = "unknown",
                      "blah"= "unknown",
                      "older than I want to be"  = "unknown",
                      "Not as old as you..."   = "unknown",
                      "Never ask a woman that question." = "unknown",  
                      "Same as yo mama" = "unknown",
                      "Too old to trick or treat without it being creepy"= "unknown",
                      "ancient"  = "unknown",
                      "Old enough to not Trick or Treat." = "unknown",
                      "49 11/12ths"  = "50",
                      "142.0" = "unknown"
                      ))

# Change age to numeric and filter strange values

unique(candy_2$age)

candy_2 <- candy_2 %>% 
  mutate(age = str_extract(age, "\\d+") %>% 
           as.numeric(age))

candy_2 <- candy_2 %>% 
  filter(age <= 100 & age >= 0)


#Candy 3 data
## Change data type to long one.
candy_3 <- candy_3 %>% 
  pivot_longer(7:112, names_to = "candies", values_to = "comments") 

##Remove unneeded columns
candy_3 <- candy_3 %>% 
  select(-7:-14)

## Rename column names
candy_3 <- candy_3 %>% 
  dplyr::rename(age = q3_age,
         going_or_not = q1_going_out,
         country = q4_country,
         province = q5_state_province_county_etc,
         gender = q2_gender)

## Change rownames
candy_3 <- candy_3 %>% 
  mutate(going_or_not = str_to_lower(going_or_not)) %>% 
  mutate(comments = str_to_lower(comments)) %>% 
  mutate(country = str_to_lower(country)) %>% 
  mutate(province = str_to_lower(province)) %>% 
  mutate(gender = str_to_lower(gender)) %>% 
  mutate(candies = str_remove_all(candies, "q[0-9]+\\_"))

## Recode observations in data
# Recode country column
candy_3 <- candy_3 %>% 
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
                          "murica" = "usa",
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
                          "united kindom" = "uk",
                          "united staes" = "usa",
                          "uae" = "unknown",
                          "usausausa"  = "usa",
                          "35"  = "unknown",
                          "unhinged states" = "usa",
                          "us of a"   = "usa",
                          "unites states"= "usa",
                          "the united states"  = "usa",
                          "north carolina"  = "usa",                                                    
                          "unied states"  = "usa",
                          "earth" = "unknown",
                          "u s"  = "usa",
                          "u.k."= "uk",
                          "the united states of america" = "usa",
                          "unite states"  = "usa",
                          "46" = "unknown",
                          "insanity lately" = "unknown",
                          "usa? hard to tell anymore.."  = "usa",
                          "'merica" = "usa",
                          "usas"  = "usa",
                          "pittsburgh"   = "usa",
                          "45" = "unknown",                                                           
                           "32"   = "unknown",                                                               
                           "a"   = "unknown",                                                                
                           "can"   = "unknown",                                                              
                           "canae"    = "unknown",                                                           
                           "new york" = "usa",
                          "california" = "usa",
                          "i pretend to be from canada, but i am really from the united states."= "usa",
                          "canada`" = "canada",                                                            
                          "scotland"  = "uk",
                          "united stated" = "usa",
                          "ahem....amerca"  = "usa",
                          "ud" = "usa",
                          "new jersey" = "usa",
                          "united ststes"  = "usa",
                          "netherland"  = "netherlands",                                                        
                           "united statss"  = "usa",                                                     
                          "endland"    = "uk",                                                        
                           "atlantis" = "unknown",                                                        
                           "murrika" = "unknown",                                                        
                           "usaa"   = "usa",                                                         
                           "alaska"  = "usa",                                                            
                           "soviet canuckistan" = "unknown",                                               
                           "n. america" = "usa",
                          "narnia" = "unknown",                                                     
                          "u s a"  = "usa",                                                             
                          "united statea"  = "usa",                                                     
                          "1"= "unknown",                                                                
                          "subscribe to dm4uz3 on youtube"  = "unknown",                                    
                          "usa usa usa!!!!" = "usa",                                                    
                          "i don't know anymore" = "unknown",                                               
                          "fear and loathing" = "unknown"      
                          ))

## Recode age column
candy_3 <- candy_3 %>% 
  mutate(age = recode(age, 
                      "Old enough to know better" = "unknown",
                      "old enough" = "unknown",
                      "As old as my tongue a few years older than my teeth"= "unknown",
                      "50s" = "50",
                      "old" = "unknown",
                      "0x2A" = "unknown",
                      "Fifty.  Nine.  Ish." = "59",
                      "Ancient" = "unknown",
                      "I remember the Nixon administration" = "unknown",
                      "over retirement age"  = "unknown",
                      "Old enough" = "unknown",
                      "50+" = "50",
                      "55+" = "55",
                      "over 40"  = "40",
                      "Hahahahahaha"= "unknown",
                      "1.0E18" = "unknown",
                      "Old"  = "unknown",
                      "Older than i act"  = "unknown",
                      "really old" = "unknown",
                      "blah"= "unknown",
                      "older than I want to be"  = "unknown",
                      "Not as old as you..."   = "unknown",
                      "Never ask a woman that question." = "unknown",  
                      "Same as yo mama" = "unknown",
                      "Too old to trick or treat without it being creepy"= "unknown",
                      "ancient"  = "unknown",
                      "Old enough to not Trick or Treat." = "unknown",
                      "49 11/12ths"  = "50",
                      "142.0" = "unknown",
                      "Many" = "unknown",
                      "?"= "unknown",
                      "no"= "unknown",
                      "45-55" = "50,",
                      "hahahahaha" = "unknown",
                      "older than dirt" = "unknown",
                      "5u"   = "unknown",                                           
                      "Enough"   = "unknown",                                       
                       "See question 2"    = "unknown",                              
                       "24-50"  = "unknown", 
                      "Over 50" = "unknown",
                      "sixty-nine" = "69",                                     
                       "46 Halloweens." = "46",
                      "OLD"= "unknown",
                      "MY NAME JEFF"  = "unknown",
                      "59 on the day after Halloween" = "59",                 
                       "your mom"     = "unknown",                                   
                       "I can remember when Java was a cool new language"= "unknown",
                       "60+" = "unknown"
                      ))

# Change age to numeric and filter strange values

unique(candy_3$age)

candy_3 <- candy_3 %>% 
  mutate(age = str_extract(age, "\\d+") %>% 
           as.numeric(age))

candy_3 <- candy_3 %>% 
  filter(age <= 100 & age >= 0)

#Remove or add columns needed for binding three data
 
 candy_3 <- candy_3 %>% 
   select(-internal_id) %>% 
   mutate(year = "2017") 


#Bind three data altogether
comb_candy_data <- rbind(candy_2, candy_3)

full_candy_data <- rbind.fill(comb_candy_data, candy_1) 
full_candy_data <- full_candy_data %>% 
  mutate(year = as.numeric(year))

#Last check before writing a clean data
glimpse(full_candy_data)

#Write clean data in csv format
write_csv(full_candy_data, "full_clean_candy_data.csv")

#Check reproducibility by writing assertive programming
full_candy_data %>% 
  verify(is.na(country) | str_detect(country, "[a-z]+")) %>% 
  verify(is.na(going_or_not) | str_detect(going_or_not, "[a-z]+")) %>% 
  verify(is.na(year) | str_detect(year, "[0-9]{4}")) %>% 
  verify(is.na(age) | str_detect(age, "[0-9]+")) 








