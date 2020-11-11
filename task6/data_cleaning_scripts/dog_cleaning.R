library(tidyverse)
library(janitor)

dog_raw <- read_csv("~/dirty_data_project/task6/raw_data/dog_survey.csv") %>% 
  clean_names()
#Remove duplicated data
dog_data <- distinct(dog_raw)
dog_data <- dog_data %>% 
  select(-10, -11)
#Check if information for a single dog per row
#Change data for two dogs
two_dogs <- dog_data %>% 
  slice(rep(119, each = 2)) 

two_dogs1 <- two_dogs[2,] %>% 
  mutate(dog_gender = recode(dog_gender, 
                             "1 male and 1 female" = "female")) %>% 
  mutate(dog_age = recode(dog_age, "5 and 4" = "4"))

two_dogs <- rbind(two_dogs, two_dogs1)

two_dogs <- two_dogs %>% 
  mutate(dog_gender = recode(dog_gender, "1 male and 1 female" = "male")) %>% 
  mutate(dog_age = recode(dog_age, "5 and 4" = "5"))

two_dogs <- distinct(two_dogs)

#Change data for three dogs
three_dogs <- dog_data %>% 
  slice(rep(174, each = 3))


three_dogs1 <- three_dogs[2,] %>% 
  mutate(dog_gender = recode(dog_gender, 
                             "M,M,F" = "male")) %>% 
  mutate(dog_age = recode(dog_age, "3,3,5" = "3")) %>% 
  mutate(dog_size = recode(dog_size, "S,L,L" = "S")) 

three_dogs2 <- three_dogs[3,] %>% 
  mutate(dog_gender = recode(dog_gender, 
                             "M,M,F" = "male")) %>% 
  mutate(dog_age = recode(dog_age, "3,3,5" = "3")) %>% 
  mutate(dog_size = recode(dog_size, "S,L,L" = "L")) 

three_dogs <- rbind(three_dogs, three_dogs1, three_dogs2)

three_dogs <- three_dogs %>% 
  mutate(dog_gender = recode(dog_gender, 
                             "M,M,F" = "female")) %>% 
  mutate(dog_age = recode(dog_age, "3,3,5" = "5")) %>% 
  mutate(dog_size = recode(dog_size, "S,L,L" = "L")) 

three_dogs <- distinct(three_dogs)


#Combine all data and delete unneded rows
comb_data <- rbind(dog_data, two_dogs, three_dogs)

comb_data <- comb_data[-c(174),]
comb_data <- comb_data[-c(119),]
#Now comb_data have dog per row

options(digits = 5)

#Clean row names
comb_data <- comb_data %>% 
  mutate(amount_spent_on_dog_food = recode(amount_spent_on_dog_food,
                                           "Between £10 and £20" = "15")) %>% 
  mutate(amount_spent_on_dog_food = str_extract(amount_spent_on_dog_food, "\\d+") %>% 
           as.numeric(amount_spent_on_dog_food))

comb_data <- comb_data %>% 
  mutate(dog_size = recode(dog_size, "Smallish" = "S",
                           "Medium sized" = "M",
                           "large" = "L",
                           "-"= "NA",
                           "N/A" = "NA",
                           "NO" = "NA"
                           ))

comb_data <- comb_data %>% 
  mutate(dog_gender = str_to_upper(dog_gender)) %>% 
  mutate(dog_gender = recode(dog_gender, "FEMLAE" = "F",
                            "FEMALE"  = "F",
                            "MALE"  = "M",
                           "-"= "NA",
                           "DON’T KNOW" = "UNKNOWN",
                           "UNKOWN" = "UNKNOWN",
                           "UNKNOWN" = "NA",
                           "—" = "NA"
  ))

comb_data <- comb_data %>% 
  mutate(dog_age = str_extract(dog_age, "\\d+") %>% 
           as.numeric(dog_age))

glimpse(comb_data)

#Write final clean data
write_csv(comb_data, "clean_dog_data.csv")

